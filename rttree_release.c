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
 * rttree_release.c
 *
 * Function to remove the LUT from an otherwise empty rttree.
 */
#include <rttree_defs.h>


/*
 * rttree_release()
 *
 * Either dispatch the root inode LUT, or complain if the tree
 * is not empty.
 */
int
rttree_release (rttree_t *rr)
{
	rtt_inode_t *ri;
	rtt_lut_t o_lut;
	const rtt_alloc_t *ra;
	rtt_stats_t *rs;

	/*
	 * Make sure the tree is empty.  An empty tree will
	 * have all three child pointers null.
	 */
	ri = &rr->rttr_root;
	if (ri->rtti_attached != RP_NULL ||
	    ri->rtti_left != RP_NULL ||
	    ri->rtti_right != RP_NULL) {
		return (RTTREE_ERR_NOT_EMPTY);
	}

	/*
	 * Fetch the existing LUT.  If its size is 2 there's
	 * nothing to do.
	 */
	o_lut = ri->rtti_lut;
	if (lut_size(o_lut) != RTT_LUT_2) {
		/*
		 * Set it to the trivial LUT, then free
		 * the existing one.
		 */
		ri->rtti_lut = lut_make(ri->rtti_child, RTT_LUT_2);
		rs = rr->rttr_stats;
		if (rs) {
			rs->rtts_luts[lut_size(o_lut)]--;
		}
		ra = rr->rttr_alloc_funcs;
		ra->rtta_lut_unref(rr, lut_size(o_lut), lut_ptr(o_lut));
	}

	return (RTTREE_OK);
}
