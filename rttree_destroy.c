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
 * rttree_destroy.c
 *
 * Function to dispatch the internal memory in an rttree.  Returns
 * a list of external nodes found in the tree.
 */
#include <rttree_update.h>

/*
 * rttree_destroy()
 *
 * Remove and free all LUTs and inodes associated with this
 * tree.  Count the external nodes, form them into one big
 * list and return that list.
 */
rtnode_t *
rttree_destroy (rttree_t *rr)
{
	rtt_inode_t *ri, *o_ri;
	rtt_lut_t lut;
	rtt_stats_t *rs;
	rtnode_t *rn_list;
	int i;

	rs = rr->rttr_stats;
	rn_list = NULL;
	o_ri = &rr->rttr_root;
	for (;;) {
		/*
		 * Head down the tree from the parent, going
		 * right when we can and left when we can't.
		 * NULL the pointer we follow so we won't
		 * go there again.
		 */
		do {
			ri = o_ri;
			o_ri = NULL;
			if (rp_is_int(ri->rtti_right)) {
				o_ri = rp_to_int(ri->rtti_right);
				ri->rtti_right = RP_NULL;
			} else if (rp_is_int(ri->rtti_left)) {
				o_ri = rp_to_int(ri->rtti_left);
				ri->rtti_left = RP_NULL;
			}
		} while (o_ri != NULL);

		/*
		 * `ri' points at something with no child inode.
		 * Count any externals to keep the statistics
		 * accurate and add the externals to the list.
		 */
		for (i = 1; i >= (-1); i--) {
			rtt_ptr_t ptr, *ptr_p;

			/*
			 * The order is right, then left, then
			 * attached, just because this is the tree
			 * lexical order.  The list as a whole won't
			 * be in any particular order, however.
			 */
			if (i < 0) {
				ptr_p = &ri->rtti_attached;
			} else {
				ptr_p = &ri->rtti_child[i];
			}
			ptr = *ptr_p;
			if (ptr != RP_NULL) {
				rtt_ptr_t p = ptr;
				rtnode_t *rn;
				uint32_t found = 0;

				*ptr_p = RP_NULL;
				do {
					rn = rp_to_ext(p);
					p = rn->rttn_next;
					found++;
				} while (p != RP_NULL);

				rn->rttn_next = rp_from_ext(rn_list);
				rn_list = rp_to_ext(ptr);

				if (rs != NULL) {
					rs->rtts_nodes -= found;
					rs->rtts_dests--;
				}
			}
		}

		/*
		 * Dump its LUT.  If it is the root it will have
		 * a NULL parent pointer and we're done.
		 */
		lut = ri->rtti_lut;
		if (lut_size(lut) != RTT_LUT_2) {
			rtt_lut_free(rr, lut);
		}
		o_ri = ri->rtti_parent;
		if (o_ri == NULL) {
			break;
		}

		/*
		 * Free the inode.  o_ri points at its parent, we'll
		 * start again from there.
		 */
		rtt_inode_free(rr, ri);
	}

	/*
	 * Clean out the root LUT pointer so no one is confused.
	 * Return the external node list we accumulated.
	 */
	ri->rtti_lut = lut_make(ri->rtti_child, RTT_LUT_2);
	return (rn_list);
}
