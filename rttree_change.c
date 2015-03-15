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
 * rttree_change.c
 *
 * Remove an external node from the tree and simultaneously add
 * a new one.  This exists so that route replacements can be done
 * without modifying the superstructure the way a delete/add
 * would.  It also handles the case that both the old and new
 * nodes are the same node, and the operation is being done because
 * a color or other change to the node that would effect its sort
 * order is in progress.  Note that the latter is generally
 * unsafe to do in a volatile tree with concurrent readers.
 */
#include <rttree_update.h>

/*
 * rttree_change()
 *
 * Do a change without coloring the node.
 */
int
rttree_change (rttree_t *rr, rtnode_t *add_rn, rtnode_t *del_rn)
{

	return (rttree_change_c(rr, add_rn, RTTREE_COLOR_NONE, del_rn));
}


/*
 * rttree_change_c()
 *
 * Add a node with the color specified, delete the old node at the
 * same time.
 */
int
rttree_change_c (rttree_t *rr, rtnode_t *add_rn, rtt_color_t add_color,
		 rtnode_t *del_rn)
{
	rtnode_t *rn, *new_rn, *old_rn, *first_rn, *d_prev_rn, *a_prev_rn;
	rtt_inode_t *ri;
	rtt_ibit_t plen;
	rtt_color_t d_color;
	int res;

	/*
	 * Search for the new node's key.  If we don't find the
	 * old node there the keys may be different or he might
	 * have given us the wrong tree.
	 */
	plen = del_rn->rttn_len;
	rn = rn_check(rr, rn_key(rr, add_rn), plen);
	if (rn == NULL) {
		return (RTTREE_ERR_NOTFOUND);
	}
	ri = rn->rttn_parent;
	if (del_rn->rttn_parent != ri) {
		return (RTTREE_ERR_NOTFOUND);
	}

	/*
	 * Find the existing guy in the list first.  Don't
	 * change anything until we've got his predecessor
	 * and successor.
	 */
	first_rn = rn;
	d_prev_rn = NULL;
	do {
		if (rn == del_rn) {
			break;
		}
		d_prev_rn = rn;
		rn = rp_to_ext(rn->rttn_next);
	} while (rn);

	if (!rn) {
		/* Not here after all?!? */
		return (RTTREE_ERR_NOTFOUND);
	}

	/*
	 * Okay, we have the old guy bracketted.  Determine where
	 * the new guy goes in the list.  If the add and delete
	 * guys are the same this can be a problem: we need to
	 * write the new color into the new guy (and old guy,
	 * if they're the same), but we might fail the operation
	 * if there is a match.  Save the old guy's color so
	 * that we can write it back if this doesn't succeed.
	 */
	d_color = rn->rttn_color;
	add_rn->rttn_len = (rtt_bit_t) plen;
	add_rn->rttn_color = add_color;
	add_rn->rttn_parent = ri;
	a_prev_rn = NULL;
	rn = first_rn;
	do {
		if (rn == del_rn) {
			continue;
		}
		res = rn_cmp_ext(rr, add_rn, rn);
		if (res == RTT_CMP_EQ) {
			/* Duplicate.  Fail this */
			del_rn->rttn_color = d_color;
			return (RTTREE_DUP);
		}
		if (res == RTT_CMP_GT) {
			break;
		}
		a_prev_rn = rn;
	} while ((rn = rp_to_ext(rn->rttn_next)) != NULL);

	/*
	 * We'll work through the possibilities here.  old_rn
	 * and new_rn will end up recording the superstructure
	 * visible change.
	 */
	old_rn = new_rn = NULL;
	if (d_prev_rn == a_prev_rn) {
		/*
		 * The two nodes fill the same spot in the list.
		 */
		add_rn->rttn_next = del_rn->rttn_next;
		RTT_WMB();
		if (d_prev_rn) {
			/* Structure change starts here */
			d_prev_rn->rttn_next = rp_from_ext(add_rn);
			/* Structure change ends here */
		} else {
			/* Structure change starts here */
			if (ri->rtti_attached == rp_from_ext(del_rn)) {
				ri->rtti_attached = rp_from_ext(add_rn);
			} else if (ri->rtti_left == rp_from_ext(del_rn)) {
				ri->rtti_left = rp_from_ext(add_rn);
			} else {
				ri->rtti_right = rp_from_ext(add_rn);
			}
			/* Structure change ends here */
			old_rn = del_rn;
			new_rn = add_rn;
		}
	} else {
		/*
		 * The nodes are separated by at least one other.
		 * Remove the deleted node before adding the new
		 * node to try to avoid creating a loop if they are
		 * the same.
		 */
		/* Structure change starts here */
		if (d_prev_rn) {
			d_prev_rn->rttn_next = del_rn->rttn_next;
		} else {
			rtt_ptr_t d_next = del_rn->rttn_next;

			if (ri->rtti_attached == rp_from_ext(del_rn)) {
				ri->rtti_attached = d_next;
			} else if (ri->rtti_left == rp_from_ext(del_rn)) {
				ri->rtti_left = d_next;
			} else {
				ri->rtti_right = d_next;
			}
			old_rn = del_rn;
			new_rn = rp_to_ext(d_next);
		}
		if (a_prev_rn) {
			add_rn->rttn_next = a_prev_rn->rttn_next;
			RTT_WMB();
			a_prev_rn->rttn_next = rp_from_ext(add_rn);
		} else {
			add_rn->rttn_next = first_rn->rttn_next;
			RTT_WMB();
			if (ri->rtti_attached == rp_from_ext(first_rn)) {
				ri->rtti_attached = rp_from_ext(add_rn);
			} else if (ri->rtti_left == rp_from_ext(first_rn)) {
				ri->rtti_left = rp_from_ext(add_rn);
			} else {
				ri->rtti_right = rp_from_ext(add_rn);
			}
			old_rn = first_rn;
			new_rn = add_rn;
		}
		/* Structure change ends here */
	}

	/*
	 * If something of note changed update the superstructure.
	 */
	if (new_rn != old_rn) {
		rttree_update_(rr, new_rn, old_rn);
	}

	/*
	 * Count it.
	 */
	rr_inc_changes(rr);
	return (RTTREE_OK);
}
