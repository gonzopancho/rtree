/*      $NetBSD: $ */

/*-
 * Copyright (c) 2014 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Dennis Ferguson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *      
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * rttree_lookup.h
 *
 * Common code for lookup functions.
 */
#ifndef RTTREE_LOOKUP_H_
#define	RTTREE_LOOKUP_H_
#include <rttree_defs.h>

/*
 * The inline is called with constant boolean arguments to customize
 * its operation.  We expect the V_* macros to be defined appropriately,
 * though we don't necessarily expect them to make much difference to
 * the generated code.
 */

static inline rtnode_t *
rn_lookup(rttree_t *, const uint32_t *, bool, rtt_ibit_t, bool,
	  rtt_colormask_t, rtt_colormask_t, bool, const void *) ALWAYS_INLINE;

static inline rtnode_t *
rn_lookup (rttree_t *rr, const uint32_t *kp, bool has_plen,
	   rtt_ibit_t plen, bool has_color,
	   rtt_colormask_t c_ignore, rtt_colormask_t c_skip,
	   bool has_compare, const void *cmp_gorp)
{
	rtnode_t *rn;
	V_ rtnode_t *v_rn;
	V_ rtt_inode_t *ri;
	rtt_ibit_t bit, d_bit, his_len, which_k;
	rtt_ptr_t r_ptr;
	uint32_t k;

	/*
	 * Search down through the LUTs looking for an
	 * external.  Stop when we get there.
	 */
	ri = rv_inode(&rr->rttr_root);
	r_ptr = RP_NULL;
	RN_KEYLUT_INIT(((has_plen && plen == 0) ? RN_NOBIT : 0),
		       kp, k, which_k);
	for (;;) {
		rtt_lut_t lut;
		uint32_t l_word;
		V_ rtt_ptr_t *lp;

		bit = ri->rtti_bit;

		/*
		 * The following block is only needed for prefix
		 * length searches, to ensure we don't get too far
		 * down the tree.  Hopefully has_plen will make
		 * it disappear when there isn't one.
		 */
		if (has_plen && bit >= plen) {
			while (bit > plen) {
				ri = rv_inode(ri->rtti_aparent);
				bit = ri->rtti_bit;
			}
			r_ptr = RP_NULL;
			break;
		}

		RN_KEYLUT(l_word, bit, kp, k, which_k);
		lut = rv_lut(ri->rtti_lut);
		lp = lut_ptr(lut);
		r_ptr = lp[l_word >> lut_ishift(lut)];
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rv_inode(rp_to_int(r_ptr));
	}

	/*
	 * The external he got might have been NULL.  Move
	 * back up the tree looking for something attached
	 * and non-NULL
	 */
	if (r_ptr == RP_NULL) {
		while ((r_ptr = ri->rtti_attached) == RP_NULL) {
			ri = rv_inode(ri->rtti_aparent);
			if (ri == NULL) {
				return (NULL);
			}
		}

		/*
		 * Catch what is sometimes a common case.  If we're
		 * just doing a straight longest match lookup and
		 * we got the default just return it.  has_color
		 * and has_compare will always be constants so the
		 * compiler should compile this down to the minimum.
		 */
		if (!has_color && !has_compare && ri->rtti_bit == 0) {
			return (rp_to_ext(r_ptr));
		}
	}

	/*
	 * That got us to an external.  Get its prefix
	 * length and do a comparison.
	 */
	rn = rp_to_ext(rv_ptr(r_ptr));
	v_rn = rn;
	his_len = v_rn->rttn_len;
	d_bit = his_len;
	if (has_plen && his_len > plen) {
		d_bit = plen;
	}
	d_bit = rn_cmp(kp, rn_key(rr, rn), d_bit);

	/*
	 * If d_bit is less than the prefix length we need to
	 * search back up the tree looking for something attached
	 * to a node testing a bit less than or equal to that.
	 * Go for it.
	 */
	ri = rv_inode(v_rn->rttn_parent);
	if (d_bit < his_len) {
		do {
			his_len = ri->rtti_bit;
			if (d_bit >= his_len &&
			    (r_ptr = ri->rtti_attached) != RP_NULL) {
				break;
			}
			ri = rv_inode(ri->rtti_aparent);
		} while (ri != NULL);
		if (ri == NULL) {
			return (NULL);
		}
		rn = rp_to_ext(rv_ptr(r_ptr));
		v_rn = rn;
	}

	/*
	 * We've got a match.  If this is an unconditional
	 * search return it.
	 */
	if (!has_color && !has_compare) {
		return (rn);
	}

	/*
	 * Not so lucky.  Scan the list of routes looking for an
	 * acceptable match.  If we don't get one here move back
	 * up the tree to a shorter prefix.
	 */
	do {
		while (rn != NULL) {
			int res = RTT_CMP_EQ;

			if (has_color) {
				res = rn_color_check(rn->rttn_color,
						     c_ignore, c_skip);
			}
			if (has_compare && res == RTT_CMP_EQ) {
				res = rn_func_check(rr, rn, cmp_gorp);
			}

			/*
			 * If res is _EQ we have a match.  If it is _LT
			 * we should look for a node further down the
			 * chain (i.e. continue in the inner loop).  If
			 * it is _GT we need to look at a shorter prefix
			 * (i.e. break to the outer loop).
			 */
			if (res > RTT_CMP_LT) {
				if (res == RTT_CMP_EQ) {
					return (rn);
				}
				break;
			}
			rn = rp_to_ext(rv_ptr(v_rn->rttn_next));
			v_rn = rn;
		}

		/*
		 * Find a shorter match.  It might be attached
		 * to his inode, otherwise try his forwarding
		 * parent.
		 */
		d_bit = ri->rtti_bit;
		if (d_bit < his_len) {
			r_ptr = ri->rtti_attached;
			his_len = d_bit;
		} else if ((ri = rv_inode(ri->rtti_aparent)) != NULL) {
			r_ptr = ri->rtti_attached;
			his_len = ri->rtti_bit;
		}
		rn = rp_to_ext(rv_ptr(r_ptr));
		v_rn = rn;
	} while (ri != NULL);

	/*
	 * Fell off the top of the tree.  Return nothing found.
	 */
	return (NULL);
}
#endif	/* RTTREE_LOOKUP_H_ */
