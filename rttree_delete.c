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
 * rttree_delete.c
 *
 * Routine to delete an external node from an rttree.
 */
#include <rttree_update.h>


/*
 * rttree_delete()
 *
 * Delete an external node from the tree.
 */
int
rttree_delete (rttree_t *rr, rtnode_t *del_rn)
{
	rtnode_t *rn, *new_rn;
	rtt_inode_t *ri, *up_ri;
	rtt_ptr_t r_ptr;
	rtt_ibit_t bit, plen;

	/*
	 * Search the tree based on the node's key, calling rn_check().
	 * We don't really have to do this, but doing both provides a
	 * check that the node is in the tree he says it is and also
	 * determines the head of the chain of nodes he's in, in case he's
	 * not first.  The external returned by rn_check() has not been
	 * prefix checked, but is the only possible match in the tree
	 * for the offered node.
	 */
	plen = del_rn->rttn_len;
	rn = rn_check(rr, rn_key(rr, del_rn), plen);
	if (rn == NULL) {
		return (RTTREE_ERR_NOTFOUND);
	}
	ri = rn->rttn_parent;
	if (del_rn->rttn_parent != ri) {
		return (RTTREE_ERR_NOTFOUND);
	}
	bit = ri->rtti_bit;

	/*
	 * `rn' is the first external to the del_rn destination. Determine
	 * if the inode has 3 child pointers, then begin to sort through
	 * the possibilities.
	 */
	up_ri = NULL;
	new_rn = NULL;
	if (rn != del_rn) {
		rtnode_t *prev_rn;

		/*
		 * This is an easy case, the node we're deleting
		 * isn't first in the list.  Find his predecessor
		 * so we can lose him.
		 */
		do {
			prev_rn = rn;
			rn = rp_to_ext(rn->rttn_next);
		} while (rn && rn != del_rn);

		if (rn == NULL) {
			/* Wasn't there after all */
			return (RTTREE_ERR_NOTFOUND);
		}

		/*
		 * We know his predecessor in the list now.  We
		 * can just stitch him out of the list and we're
		 * done with no collateral damage.
		 */
		/* Structure change starts here */
		prev_rn->rttn_next = rn->rttn_next;
		/* Structure change ends here */

		/*
		 * Remember that this had no effect on the superstructure.
		 */
		del_rn = NULL;
	} else if ((r_ptr = rn->rttn_next) != RP_NULL) {
		/*
		 * He's first on his list but has a successor
		 * replacement.  The inode is staying and we'll fill in
		 * his spots with his successor in the list by virtue of
		 * setting new_rn to that.
		 *
		 * Note the new guy in his spot.
		 */
		new_rn = rp_to_ext(r_ptr);
	} else if (bit != 0 && (ri->rtti_attached == RP_NULL ||
				ri->rtti_left == RP_NULL ||
				ri->rtti_right == RP_NULL)) {

		/*
		 * In this case the inode has two child pointers and
		 * isn't the permanent root inode, we're removing one
		 * of them and the inode is hence coming out of the tree.
		 * Mark the inode deleted for superstructure maintenance.
		 */
		ri_mark_deleted(ri);

		/*
		 * We need to come out of here with up_ri set to
		 * the deleted node's parent.  We'll then do the
		 * work in two stages.  We'll remove the reference to
		 * the deleted node right away, but defer removing
		 * the inode from the structure until after the
		 * superstructure update below.  up_ri will be the
		 * signal for the code below to complete the work.
		 */
		up_ri = ri->rtti_parent;
	} else {
		/*
		 * In the other cases the inode stays but we NULL
		 * the pointer that refers to the guy.
		 */
	}

	/*
	 * If something is being replaced in the inode deal with
	 * that now.
	 */
	if (del_rn != NULL) {
		/* Structure change starts here */
		if (bit == plen) {
			ri->rtti_attached = rp_from_ext(new_rn);
		} else if (ri->rtti_left == rp_from_ext(del_rn)) {
			ri->rtti_left = rp_from_ext(new_rn);
		} else {
			ri->rtti_right = rp_from_ext(new_rn);
		}
		/* Structure change ends here */

		rttree_update_(rr, new_rn, del_rn);
	}

	/*
	 * If we've got a partially deleted inode, complete that
	 * work now.
	 */
	if (up_ri) {
		/*
		 * Find the remaining node in the inode we're deleting.
		 */
		if (ri->rtti_attached != RP_NULL) {
			r_ptr = ri->rtti_attached;
		} else if (ri->rtti_left != RP_NULL) {
			r_ptr = ri->rtti_left;
		} else {
			r_ptr = ri->rtti_right;
		}

		/* Structure change starts here */
		rp_reparent(r_ptr, up_ri);
		RTT_WMB();	/* Make sure this is really done first. */
		if (up_ri->rtti_left == rp_from_int(ri)) {
			up_ri->rtti_left = r_ptr;
		} else {
			up_ri->rtti_right = r_ptr;
		}
		/* Structure change ends here */

		/*
		 * The inode is now fully untethered.  Make sure
		 * everyone thinks so, then free it.
		 */
		RTT_WMB();
		rtt_inode_unref(rr, ri);
	}

	/*
	 * The last job to do is accounting.
	 */
	rr_dec_nodes(rr);
	if (del_rn != NULL && new_rn == NULL) {
		rr_dec_dests(rr);
	}
	rr_inc_deletes(rr);
	return (RTTREE_OK);
}
