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
 * rttree_init.c
 *
 * Initialization functions for the rttree_t root structure.
 */
#include <rttree_update.h>


/*
 * rtt_keyptr_make()
 *
 * Inline to format an rtt_keyptr_t given a pointer and key offset.
 * They need to have been checked.
 */
static inline rtt_keyptr_t
rtt_keyptr_make (size_t p_offset, size_t k_offset)
{
	rtt_keyptr_t pk;

	pk = (rtt_keyptr_t) (p_offset / RN_PTR_SIZE) << RN_PTR_SHIFT;
	pk |= (rtt_keyptr_t) (k_offset / RN_KEY_SIZE);

	return (pk);
}


/*
 * rttree_init()
 *
 * Initialize an rttree_t root structure.
 */
int
rttree_init (rttree_t *rr, size_t p_offset, size_t k_offset, rtt_size_t lut_s,
	     rtt_colormask_t m_color, const rtt_alloc_t *alloc_p,
	     const rtt_cmpfuncs_t *cmp_p, rtt_stats_t *stats_p)
{
	rtt_inode_t *ri;
	rtt_ptr_t *lut_p;

	/*
	 * Validate the arguments before we do anything else.
	 * Complain at the first sign of trouble.  Make sure
	 * that stuff we need to have is there.
	 */
	if (rr == NULL || alloc_p == NULL ||
	    alloc_p->rtta_inode_alloc == NULL ||
	    alloc_p->rtta_inode_free == NULL ||
	    alloc_p->rtta_inode_unref == NULL ||
	    alloc_p->rtta_lut_alloc == NULL ||
	    alloc_p->rtta_lut_free == NULL ||
	    alloc_p->rtta_lut_unref == NULL ||
	    (cmp_p != NULL &&
	      (cmp_p->rttc_cmp == NULL || cmp_p->rttc_match == NULL))) {
		return (RTTREE_ERR_PARAM);
	}

	/*
	 * Check the alignment of the root inode structure.
	 */
	ri = &rr->rttr_root;
	if (!lut_align_ok(ri->rtti_child)) {
		return (RTTREE_ERR_ROOT_ALIGN);
	}

	/*
	 * Check the key and pointer offsets.  They must be appropriately
	 * aligned, not too large, and not pointing at memory in the
	 * node itself.
	 */
	if (!RN_KEY_ALIGN_OK(k_offset) || k_offset > RN_KEY_MAX ||
	    (p_offset == RTTREE_PTR_NONE && k_offset < RN_MIN_OFFSET)) {
		return (RTTREE_ERR_KEY_OFFSET);
	}
	if (!RN_PTR_ALIGN_OK(p_offset) || p_offset > RN_PTR_MAX ||
	    (p_offset != RTTREE_PTR_NONE && p_offset < RN_MIN_OFFSET)) {
		return (RTTREE_ERR_PTR_OFFSET);
	}

	/*
	 * Make sure LUT size in range.  Don't do the memory
	 * allocation until the end; if it fails we won't worry
	 * about it since it is certain to be retried.
	 */
	if (lut_s > RTT_LUT_MASK) {
		return (RTTREE_ERR_LUT_SIZE);
	}
	/*
	 * Looks like we need to go ahead.  Everything in the
	 * inode needs to be zeroed except the LUT pointer, which
	 * we'll fill in with the local version.
	 */
	ri->rtti_child[0] = RP_NULL;
	ri->rtti_child[1] = RP_NULL;
	ri->rtti_lut = lut_make(ri->rtti_child, RTT_LUT_2);
	ri->rtti_bit = 0;
	ri->rtti_byte = 0;
	ri->rtti_counts[0] = ri->rtti_counts[1] = 0;
	ri->rtti_counts[2] = ri->rtti_counts[3] = ri->rtti_counts[4] = 0;
	ri->rtti_parent = NULL;
	ri->rtti_aparent = NULL;
	ri->rtti_attached = RP_NULL;

	/*
	 * Work on the remainder of the root structure.
	 */
	rr->rttr_offsets = rtt_keyptr_make(p_offset, k_offset);
	rr->rttr_color_mask = m_color;
	rr->rttr_root_lut = lut_s;
	rr->rttr_opt = rttree_opt_timid;

	if (cmp_p) {
		rr->rttr_cmp = cmp_p->rttc_cmp;
		rr->rttr_match = cmp_p->rttc_match;
	} else {
		rr->rttr_cmp = NULL;
		rr->rttr_match = NULL;
	}
	rr->rttr_alloc_funcs = alloc_p;

	/*
	 * Remember the stats pointer.  We don't initialize them since
	 * they may count stats for more than just our tree.
	 */
	rr->rttr_stats = stats_p;

	/*
	 * Finally, fill in the lookup table.  If the memory
	 * allocation fails we'll do without.
	 */
	if (lut_s != RTT_LUT_2 &&
	    (lut_p = alloc_p->rtta_lut_alloc(rr, lut_s)) != NULL) {
		unsigned int i;

		for (i = RTT_LUT_ENTRIES(lut_s); i > 0; ) {
			lut_p[--i] = RP_NULL;
		}
		ri->rtti_lut = lut_make(lut_p, lut_s);
		ri->rtti_counts[CNT_HAS_LUT] |= RI_HAS_LUT;
		if (stats_p) {
			stats_p->rtts_luts[lut_s] += 1;
		}
	}

	/*
	 * All done.
	 */
	return (RTTREE_OK);
}
