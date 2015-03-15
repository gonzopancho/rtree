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
 * rttree_tune.c
 *
 * Replace the optimization schedule constants for this tree
 * with those provided.
 */
#include <rttree.h>

/*
 * rttree_tune()
 *
 * Check that the tuning constants he offers are vaguely sensible,
 * then record them in the tree root.
 */
int
rttree_tune (rttree_t *rr, const rtt_opt_t *r_opt)
{
	unsigned int i;

	/*
	 * If he didn't include a pointer to something just
	 * complain and do nothing.
	 */
	if (r_opt == NULL) {
		return (RTTREE_ERR_PARAM);
	}

	/*
	 * Check the values.  The only thing we insist on is that
	 * the low water mark be no larger than the high water mark.
	 */
	for (i = 0; i < RTT_LUT_NUM; i++) {
		if (r_opt[i].lo > r_opt[i].hi) {
			return (RTTREE_ERR_INVALID);
		}
	}

	rr->rttr_opt = r_opt;
	return (RTTREE_OK);
}
