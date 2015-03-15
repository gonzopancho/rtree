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
 * rttree_find.h
 *
 * Common code for find functions.
 */
#ifndef RTTREE_FIND_H_
#define	RTTREE_FIND_H_
#include <rttree_defs.h>

/*
 * A _find() operation finds a node with the specified prefix and
 * prefix length.  If one does not exist it returns NULL.
 *
 * The inline is called with constant boolean arguments to customize
 * its operation.  We expect the V_* macros to be defined appropriately,
 * though we don't necessarily expect them to make much difference to
 * the generated code.
 */

static inline rtnode_t *
rn_find(rttree_t *, const uint32_t *, unsigned int, bool,
	  rtt_colormask_t, rtt_colormask_t, bool, const void *) ALWAYS_INLINE;

static inline rtnode_t *
rn_find (rttree_t *rr, const uint32_t *kp, unsigned int prefix_len,
	 bool has_color, rtt_colormask_t c_ignore, rtt_colormask_t c_skip,
	 bool has_compare, const void *cmp_gorp)
{
	rtnode_t *rn;
	V_ rtnode_t *v_rn;
	V_ rtt_inode_t *ri;
	rtt_ibit_t bit, which_k;
	const rtt_ibit_t plen = prefix_len;
	rtt_ptr_t r_ptr;
	uint32_t k;

	/*
	 * Search down through the LUTs looking for an
	 * external with the correct prefix length.  If
	 * the node he's looking for is a child we'll arrive
	 * at it directly.  If it is attached, however, we may
	 * move past it towrds the children, so if we get to an
	 * inode testing bits beyond the prefix we need to move
	 * back up looking for an attached route.
	 */
	ri = rv_inode(&rr->rttr_root);
	if (plen == 0) {
		/*
		 * Default prefix.  Get to the point.
		 */
		r_ptr = ri->rtti_attached;
	} else {
		RN_KEYLUT_INIT(0, kp, k, which_k);
		for (;;) {
			rtt_lut_t lut;
			uint32_t l_word;
			V_ rtt_ptr_t *lp;

			bit = ri->rtti_bit;
			if (bit >= plen) {
				while (bit > plen) {
					ri = rv_inode(ri->rtti_aparent);
					bit = ri->rtti_bit;
				}
				r_ptr = ri->rtti_attached;
				break;
			}

			RN_KEYLUT(l_word, bit, kp, k, which_k);
			lut = rv_lut(ri->rtti_lut);
			lp = lut_ptr(lut);
			r_ptr = lp[l_word >> lut_ishift(lut)];
			if (rp_is_ext(r_ptr)) {
				break;
			}
			ri = rp_to_int(rv_ptr(r_ptr));
		}
	}

	/*
	 * At this point r_ptr points at an external.  If the external
	 * is NULL we have no match.  If the prefix is too long there
	 * might be a match attached further up the tree.  In the
	 * latter case if we don't find an inode with a bit equal
	 * to the prefix length there is no match.
	 */
	if (r_ptr == RP_NULL) {
		return (NULL);
	}
	rn = rp_to_ext(rv_ptr(r_ptr));
	v_rn = rn;
	bit = v_rn->rttn_len;
	if (bit > plen) {
		ri = rv_inode(v_rn->rttn_parent);
		while ((bit = ri->rtti_bit) > plen) {
			ri = rv_inode(ri->rtti_aparent);
		}
		r_ptr = ri->rtti_attached;
		if (r_ptr == RP_NULL) {
			return (NULL);
		}
		rn = rp_to_ext(rv_ptr(r_ptr));
		v_rn = rn;
	}

	/*
	 * If this node isn't a prefix match we're through.
	 * If it is but a key comparison finds a mismatch
	 * we're also through.
	 */
	if (bit != plen || rn_cmp(kp, rn_key(rr, rn), plen) < plen) {
		return (NULL);
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
	 * acceptable match.  If we find an ignore or fall off
	 * the end of the list we're done.
	 */
	do {
		int res = RTT_CMP_EQ;

		if (has_color) {
			res = rn_color_check(rn->rttn_color,
					     c_ignore, c_skip);
		}
		if (has_compare && res == RTT_CMP_EQ) {
			res = rn_func_check(rr, rn, cmp_gorp);
		}
		if (res >= RTT_CMP_EQ) {
			if (res == RTT_CMP_EQ) {
				return (rn);
			}
			break;
		}
		rn = rp_to_ext(rv_ptr(v_rn->rttn_next));
		v_rn = rn;
	} while (rn != NULL);

	/*
	 * Fell off the end of the list or got an ignore.  Return no match.
	 */
	return (NULL);
}
#endif	/* RTTREE_FIND_H_ */
