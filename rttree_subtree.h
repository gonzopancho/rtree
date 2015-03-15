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
 * rttree_subtree.h
 *
 * Common code for rttree_sub_first*() and rttree_sub_last*() functions.
 */
#ifndef RTTREE_SUBTREE_H_
#define	RTTREE_SUBTREE_H_
#include <rttree_next.h>

/*
 * The inline is called with constant boolean arguments to customize
 * its operation.  We expect the V_* macros to be defined appropriately,
 * though we don't necessarily expect them to make much difference to
 * the generated code.  The function provides support for the basic
 * _first() and _last() search, returning the first node
 */
static inline rtnode_t *
rn_subtree(rttree_t *, const uint32_t *, rtt_ibit_t, rtt_ibit_t) ALWAYS_INLINE;

static inline rtnode_t *
rn_subtree (rttree_t *rr, const uint32_t *kp, rtt_ibit_t st_bit, rtt_ibit_t dir)
{
	rtnode_t *rn;
	V_ rtt_inode_t *ri;
	rtt_ibit_t bit, d_bit, his_len, which_k;
	rtt_ptr_t r_ptr;
	rtt_lut_t lut;
	uint32_t k;

	/*
	 * Search down through the LUTs looking for a
	 * place past our bit.  Stop when we get there.
	 */
	lut = 0;
	ri = rv_inode(&rr->rttr_root);
	r_ptr = RP_NULL;
	RN_KEYLUT_INIT(RN_NOBIT, kp, k, which_k);
	while ((bit = ri->rtti_bit) < st_bit) {
		uint32_t l_word;
		V_ rtt_ptr_t *lp;

		RN_KEYLUT(l_word, bit, kp, k, which_k);
		lut = rv_lut(ri->rtti_lut);
		lp = lut_ptr(lut);
		r_ptr = lp[l_word >> lut_ishift(lut)];
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rp_to_int(rv_ptr(r_ptr));
	}

	/*
	 * If we stopped late we may need to go back up
	 * the tree.  If we stopped early we may need to
	 * do a bit\-by\-bit search down.
	 */
	RN_KEYLUT_TO_KEYBIT(which_k);
	if (bit >= st_bit) {
		/*
		 * Move back up to the first node testing
		 * a bit at or beyond st_bit.  We're probably
		 * there but we need to check since a LUT may
		 * span the bit.
		 */
		while (bit > st_bit) {
			V_ rtt_inode_t *p_ri;
			rtt_ibit_t p_bit;

			p_ri = rv_inode(ri->rtti_parent);
			p_bit = p_ri->rtti_bit;
			if (p_bit < st_bit) {
				break;
			}
			bit = p_bit;
			ri = p_ri;
		}
	} else {
		rtt_ibit_t max_bit;

		/*
		 * We stopped early, which means that r_ptr points
		 * either at an external or NULL.  We kept a copy
		 * of the lut that got us here above, so determine
		 * how many bits that covers.
		 */
		max_bit = bit + lut_isize(lut);
		if (max_bit < st_bit) {
			/*
			 * LUT tested all bits in the prefix.  If we got a
			 * NULL there's no hope.  Otherwise r_ptr is our
			 * only hope.
			 */
			if (r_ptr == RP_NULL) {
				return (NULL);
			}
			goto got_one;
		}

		/*
		 * We tested bits beyond st_bit.  Search down from
		 * ri looking for the first inode testing a bit
		 * beyond st_bit.
		 */
		do {
			rtt_ibit_t b_dir;

			RN_KEYBIT(b_dir, bit, kp, k, which_k);
			r_ptr = ri->rtti_child[b_dir];
			if (rp_is_ext(r_ptr)) {
				if (r_ptr == RP_NULL) {
					/*
					 * No hope.
					 */
					return (NULL);
				}
				goto got_one;
			}
			ri = rp_to_int(rv_ptr(r_ptr));
		} while ((bit = ri->rtti_bit) < st_bit);
	}

	/*
	 * At this point we have the first inode testing a bit >= st_bit.
	 * We need to travel in our preferred direction.  The problem
	 * with this is if the direction is RTT_LEFT we need to find
	 * an attached node if we see one, while if the direction is
	 * RTT_RIGHT that's the last thing we want.  `dir' is a constant
	 * so hopefully we get good code from this.
	 */
	for (;;) {
		rtt_ptr_t a_ptr = ri->rtti_attached;

		if (dir == RTT_LEFT && a_ptr != RP_NULL) {
			r_ptr = a_ptr;
			break;
		}
		r_ptr = ri->rtti_child[dir];
		if (r_ptr == RP_NULL) {
			r_ptr = ri->rtti_child[RN_NOT(dir)];
			if (r_ptr == RP_NULL) {
				if (dir == RTT_RIGHT && a_ptr != RP_NULL) {
					r_ptr = a_ptr;
					break;
				}
#ifndef	RTTREE_VOLATILE
				/*
				 * We're at the root of an empty, non-volatile
				 * tree.  Return the failure.
				 */
				return (NULL);
#else	/* RTTREE_VOLATILE */
				/*
				 * XXX This code exists to try to get
				 * a better result when more than one
				 * concurrent modification is made while
				 * we're in here.  The user shouldn't
				 * let that happen...
				 *
				 * All three pointers are NULL.  If
				 * we're at the root inode we'll assume
				 * the tree is empty.  Otherwise it
				 * is an unlikely transient and we can
				 * just retry.
				 */
				if (__predict_true(bit == 0)) {
					return (NULL);
				}
				continue;
#endif	/* RTTREE_VOLATILE */
			}
		}

		/*
		 * Got something.  If it is an external we're
		 * through, otherwise go around again.
		 */
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rp_to_int(rv_ptr(r_ptr));
		bit = ri->rtti_bit;
	}

	/*
	 * That got us to an external.  Get its prefix
	 * length and do a comparison.
	 */
got_one:
	rn = rp_to_ext(rv_ptr(r_ptr));
	his_len = rn->rttn_len;
	if (his_len < st_bit) {
		/*
		 * He's not in the subtree.
		 */
		return (NULL);
	}
	d_bit = rn_cmp(kp, rn_key(rr, rn), st_bit);
	if (d_bit < st_bit) {
		/*
		 * He's a mismatch.  There's no hope
		 */
		return (NULL);
	}
	return (rn);
}


/*
 * rn_sub_first_cx()
 *
 * Perform a colored/matched _sub_first*() operation.  Calls the inline
 * above to find the first possible match, then calls rn_next_cx() to
 * do the matching.
 */
static inline rtnode_t *
rn_sub_first_cx (rttree_t *rr, const uint32_t *kp, rtt_ibit_t st_bit,
		 bool has_color, rtt_colormask_t c_i, rtt_colormask_t c_s,
		 bool has_gorp, const void *cmp_gorp)
{
	rtnode_t *rn;

	rn = rn_subtree(rr, kp, st_bit, RTT_LEFT);
	if (rn) {
		rn = rn_next_cx(rr, rn, false, st_bit, true, has_color,
				c_i, c_s, has_gorp, cmp_gorp);
	}
	return (rn);
}


/*
 * rn_sub_last_cx()
 *
 * Perform a colored/matched _sub_last*() operation.  Calls the inline
 * above to find the first possible match, then calls rn_prev_cx() to
 * do the matching.
 */
static inline rtnode_t *
rn_sub_last_cx (rttree_t *rr, const uint32_t *kp, rtt_ibit_t st_bit,
		bool has_color, rtt_colormask_t c_i, rtt_colormask_t c_s,
		bool has_gorp, const void *cmp_gorp)
{
	rtnode_t *rn;

	rn = rn_subtree(rr, kp, st_bit, RTT_RIGHT);
	if (rn) {
		rn = rn_prev_cx(rr, rn, false, st_bit, true, has_color,
				c_i, c_s, has_gorp, cmp_gorp);
	}
	return (rn);
}

#endif	/* RTTREE_SUBTREE_H_ */
