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
 * rttree_update.c
 *
 * Updates rttree optimization structures to account for a change
 * in the basic structure.  Supplies one entry point, rttree_update_(),
 * to do this.
 */
#include <rttree_update.h>

/*
 * The basic rttree structure stores external nodes (routes) with
 * associated prefixes in a search structure of inodes.  The latter
 * consist of a bit (-to-test) number and 4 pointers.  Each of the two
 * child pointers can point at either another inode with a larger bit
 * number (i.e. testing a less significant prefix bit) or at an
 * external node with a prefix length larger than the inode's bit
 * number.  The third, attached pointer may point at an external node
 * with a prefix length equal to the inode's bit number.  The fourth
 * pointer is a parent pointer which points at the inode (having a bit
 * number less than the current bit number) with a child pointer pointing
 * to the current one.  The tree is maintained so that the external
 * nodes in the subtree below any inode all share a prefix to the
 * length of the inode's bit number, the same prefix that a node
 * attached to that inode would have.  Any one of an inode's down
 * pointers or attached pointer may be NULL, but an inode is not
 * added to the tree until at least two of these three pointers
 * is non-NULL and is removed from the tree if a second pointer
 * becomes NULL.  In the current implementation the exception to this
 * rule is the root inode (with a bit number of 0) which is maintained
 * in the tree permanently; this is also the only inode with a NULL
 * parent pointer.
 *
 * A longest matching prefix search in this structure is performed by
 * traversing the inodes from the tree root using the bit from the search
 * key indicated by each inode's bit number to select one of the inode's
 * child pointers.  The search continues until a child pointer to an
 * external node is found (or a NULL child pointer, in which case the
 * external attached to the inode is "found" instead).  The search key
 * is compared to the external node's prefix to determine the number
 * of bits of prefix they have in common.  If they match to the full
 * length of the "found" external node's prefix then this external
 * node has the longest matching prefix in the tree (if an external
 * with a longer matching prefix existed the search would have found
 * that instead).  Otherwise the search continues by searching back
 * up the tree, through the inode parent pointers, looking for an inode
 * with a bit number less than or equal to the common prefix length
 * determined by the comparison and having an attached external
 * node.  The external attached to the first such node found has the
 * longest matching prefix in the tree.  Clearly an external node with
 * a zero-length prefix (i.e. a default route) will match all search
 * keys but as a node with a prefix length of 0 would be attached
 * to the root inode, with a bit number of 0, and the upward search
 * for a match visits inodes with progressively smaller bit numbers,
 * the root inode is the last place searched and a default match will
 * only be found if no longer match is present in the tree.
 *
 * Incremental maintenance of this basic structure is performed by
 * rttree_add(), rttree_delete() and rttree_change(), and is quite simple
 * and straight forward.  When an external with a new prefix is added
 * to the tree a single inode will usually (but not always) be stitched
 * into the tree to point at it while when an external is deleted an
 * inode will usually (but not always) be removed.  Finding where to
 * add a new external node and its associated inode is done with a
 * search using the new external's prefix as a search key, so the
 * scaling properties of adds to the basic structure is identical
 * to the scaling of searches.  Deletes from the basic structure
 * could be done with O(1) work but, for sanity, a search of the tree
 * for the prefix of the external node being deleted is done anyway
 * to ensure the external is in fact present in the tree the caller
 * indicated it was being deleted from.  Changes, which replace
 * one external with another having the same prefix, are fairly
 * trivial to implement but again perform a sanity search, so the
 * cost of individual modification operations on the structure scales
 * as the cost of a search of the structure.  The cost of a search
 * in the basic structure described above, in a tree with `n' unique
 * prefixes, will be about log(n) individual bit tests on average for
 * the downward traversal of the tree, then a prefix comparison whose
 * cost depends on the length of the prefix, followed by a traversal
 * of between 0 and log(n) inodes back up the tree to find the longest
 * match.  That is, the work of both the downward and upward stages of
 * the search generally scale as O(log(n)).
 *
 * Two improvements to the basic search structure, adding two additional
 * pointers and some additional bookkeeping overhead to each inode, are
 * implemented, with their maintenance being performed by rttree_update_()
 * after a change to the basic structure has been made.  The simpler of
 * these improves the upward stage of the search.  While an upward search
 * through the basic parent pointers can require traversing log(n) inodes
 * to find a result (when the search key is matched only by the default
 * route, which is sometimes a very common case) it is the case that the
 * only inodes traversed which are interesting to the search are those
 * having attached external nodes, and while log(n) can be quite large
 * in real Internet routing tables it is quite unusual for even 3 or 4
 * of these inodes to have an attached external; generally there will
 * be only 1 or 2.  We can hence substantially reduce the cost of upward
 * searches by maintaining in each inode an "attached parent" pointer which
 * points not at the immediate predecessor inode in the tree, as the parent
 * pointer does, but rather at the nearest ancestor with an attached
 * external.  When an upward search is necessary this allows a result to
 * be found while traversing a very small number of inodes.  The cost of
 * this additional structure is that modifications involving the addition
 * or deletion of an attached node may require rttree_update_() to visit
 * a large number of inodes to appropriately modify the effected attached
 * parent pointers.  We mitigate the worst case of this (changes to
 * the default route, which might require visiting nearly every inode
 * in the tree) by permanently maintaining a root inode testing bit 0 in
 * the tree and always including this inode at the tail of the attached
 * parent chain even when no default node is present.  Beyond this, however,
 * the fact that lookup operations in a route table are far more common
 * than route additions or deletions means the potential exposure to
 * occasionally expensive modifications is well paid for by the
 * improvement in lookup performance.
 *
 * The second improvement is to the downward search performance, and
 * improves the scaling of these searches to better than O(log(n)).  Note
 * that the basic search proceeds through each inode by using a single bit
 * from the search key at each inode as an index to a 2 entry array of
 * pointers to select the pointer to the structure to visit next.  On
 * average it takes log(n) single bit indexes to find an external to test
 * against.  The improvement is to generalize this by adding variable
 * sized (but always a power of 2) arrays of pointers to certain inodes
 * indexed by a variable number of bits from the search key, this having
 * the effect of producing the same result with a single table index that
 * traversing the inodes using the same bits for single bit selections
 * would yield.  This array is called a "Lookup Table", or LUT, and both
 * the memory location and the size of the LUT is encoded into the inode's
 * LUT pointer (the pointer and the size are squeezed into the pointer-sized
 * space to allow atomic changes to the LUT, necessary if there are
 * concurrent searches in progress).  To ensure that the work of modifying LUTs
 * is limited and incremental, and to simplify dealing with endianness issues,
 * the size of an LUT index is limited to between 1 and 8 bits, all of which
 * are required to come from a single byte in the search key.  Furthermore,
 * while 2-8 bit LUTs require a memory allocation separate from the inode
 * each is attached to, a 1 bit LUT pointer simply points at the 2-child
 * -pointer array in the same inode.  This makes LUT memory allocations
 * (unlike inode memory allocations) best-effort; in the extreme case that
 * all LUT memory allocations fail lookups will still produce correct results
 * with the same amount of work (O(log(n))) as a basic lookup, while any
 * successful LUT allocations will make this better still.
 *
 * Luts improve searches in two ways.  A multi-bit table index clearly
 * does in one operation what would require several single-bit indexes
 * to accomplish, so an LUT removes inodes from the search path.  Also,
 * since some of the bit combinations used to index a larger LUT are
 * ones that would not be tested in a basic bit-by-bit search the LUT
 * lookup will detect a greater proportion of searches which aren't
 * going to find a long prefix match further down the tree (by returning
 * a NULL pointer, an uncommon result when basic bit-by-bit searches
 * are done), allowing those searches to turn upward earlier.  Note
 * that when the upward turn is found early enough that the first attached
 * parent is the default route (an external with a 0-length prefix) this
 * also eliminates the need to do a prefix match, instead allowing the
 * default result to be returned immediately.
 *
 * Despite the constraints, maintaining LUTs is still a bit daunting,
 * particularly since we need to marshall changes in a way which keeps
 * lookups concurrently in progress while the changes are being made
 * from returning an "incorrect" result ("incorrect" in this case means
 * that when a modification is made concurrent lookups should return
 * either the pre-modification result or the post-modification result
 * and nothing else).  The basic rules of thumb are that, when things
 * are otherwise equal, we prefer to place LUTs on inodes closer to
 * the root of the tree since these will benefit a greater number of
 * searches (a LUT at the tree root is accessed by every search; LUTs
 * further down are only accessed with a search happens to go that way,
 * with the fraction of lookups "going that way" generally decreasing with
 * increased distance from the root) and we choose the size of the LUT based
 * on the number of single-bit tests the presence of a LUT of that size would
 * make unnecessary (i.e. the number of inodes eliminated from the search).
 * The latter rule makes the space used for LUTs roughly proportional to the
 * number of inodes in the structure, a number which is in turn proportional
 * to the number of external prefixes stored in the tree (i.e. `n'), keeping
 * the memory consumption of the internal data structure O(n).
 *
 * This still leaves complexities to deal with.  When a >1 bit LUT is added
 * to an inode the inodes below it which are now removed from the search
 * path (these are called "covered" inodes) have no need for a >1 bit LUT
 * themselves since they won't be used, so LUTs on newly covered inodes
 * can be (and should be) freed.  When an inode is added to the tree by
 * rttree_add() we first check whether its presence has made it worth while
 * to grow a LUT on an inode further up the tree to cover it; if not we
 * check whether it would be useful to add an LUT to the new inode itself.
 * Whenever a LUT is added or extended this may result in inodes with LUTs
 * further down the tree becoming covered, so these LUTs need to be removed,
 * but removing those LUTs may uncover inodes even further down the tree
 * and some of these may need LUTs added to maintain search performance.  To
 * maintain the performance of concurrent searches we add LUTs where they are
 * needed before the LUT additions or extensions above them are made while
 * deferring LUT removals until after the change, so changes to the LUT
 * structure ripple from the bottom up with new LUTs being added first while
 * now-unncessary LUTs are deleted last.  When an inode is to be deleted
 * by rttree_delete() (which calls rttree_update_() prior to actually removing
 * the inode from the structure) the work to be done is a pretty much identical
 * sequence of LUT additions, deletions and/or size changes except that it is
 * initiated by the shrinkage or removal of an LUT higher in the tree rather
 * than its addition or extension.
 *
 * Note that for an entirely unconstained arrangement of LUTs a single change
 * close to the root of the tree might be (rarely) expected to cause a
 * rearrangement of all the LUTs in the subtree below that point; a change
 * to the root inode's LUT might cause all other LUTs in the tree to change.
 * This possibility is eliminated by (arbitrary) constraint that the index to
 * an LUT index be extracted from a single search key byte, with the bits not
 * crossing byte boundaries.  This limits the effect of any change to a region
 * of the tree having at most 255 inodes testing bits in a single search key
 * byte, a much more modest problem.  It should also be made clear that it
 * is only necessary to rearrange LUTs in response to a minority of changes
 * to the basic structure.  Most changes will be implemented by simply
 * rewriting pointers in existing LUTs (in fact, generally just one pointer
 * in one LUT) without changing their arrangement.  On the rarer occasions
 * when a modification of LUTs is necessary, however, the byte constraint
 * ensures a fairly modest upper limit to the work to be done in response
 * to a single change.
 *
 * The scaling of this lookup structure with the number of external prefixes
 * in the tree is a question whose answer I am unsure of.  I believe that
 * if the LUT size and byte-crossing constraints were eliminated, and the
 * only constraint imposed on LUTs were that the memory consumption of the
 * structure remain O(n), then the lookup performance of the structure would
 * be O(log(log(n))); adding an external prefix to the tree increases the
 * number of bits that need to be tested to find a result but also increases
 * the size of the LUTs and hence the number of bits consumed by each table
 * dereference.  This is an extremely good result for a fully general longest
 * match structure consuming O(n) memory without any other constraints
 * (for example, there is no need for a priori knowledge of the size of `n',
 * the structure is incrementally self-scaling).  Note that I expect the
 * same-byte constraint to impair the scalability of the structure but it
 * will still scale better, and perform significantly better, than the
 * O(log(n)) bit-by-bit basic search.
 *
 * With this background in mind an overview of what is done in here can be
 * provided.  A modification to the tree is done to add and/or remove an
 * external node from the tree.  rttree_change() replaces an external with
 * another having the same prefix (requiring no change to the basic inode
 * structure).  rttree_add() adds an external to the basic structure, and may
 * add a new inode as well; when it does the latter it flags the new inode
 * by setting the "target" bit.  rttree_delete() removes an external from
 * the basic structure, and may require the deletion of an inode as well.  In
 * the latter case it flags the inode with the "target" and "delete" bits,
 * but leaves the deleted inode in the tree for the call to rttree_update_();
 * since it has removed the reference to the deleted external the inode will
 * temporarily remain in the basic structure as a one-way branch, which
 * concurrent searches might notice and need to deal with.
 *
 * rttree_update_() is called with the above work to the basic structure done
 * and pointers to two externals, an "old" one which has had references
 * removed from the basic structure and a "new" one which has had references
 * newly added to the basic structure.  When both pointers are non-NULL
 * the operation is a change (which, given the multiple same-prefix design
 * of the structure, may occur in _add() and _delete() as well as _change()
 * operations) and no change to the inode structure will have been made.
 * Otherwise one of the pointers will be NULL, the operation is an add or
 * delete and the inode which the external is (or was) referred to from may
 * (or may not) be a "target".  The job of rttree_update_() is to complete
 * the work of updating LUTs testing more than one bit (the two-way LUTs
 * are shared with the basic structure, and so are already updated) and
 * the attached parent pointers.
 *
 * The first step is to call rn_replace(), which corrects any references
 * directly to the external nodes in the existing LUTs as well as updating
 * attached parent pointers when an attached external is added or deleted.
 * On return from this function the attached parent pointers and LUT
 * structures will have been adjusted to fully implement the change with
 * the sole exception that, if there is a "target" inode, there may be
 * a single pointer in a LUT which still points to the deleted inode or
 * doesn't yet point to the newly-added inode.
 *
 * The second step is to call lut_optimize() with a pointer to the inode
 * associated with the changing external(s), which may or may not have
 * the target bit set.  lut_optimize() looks at or above this inode in
 * the tree to find the inode closest to the root of the tree with an LUT
 * whose size is no longer appropriate given the current inode configuration
 * in the basic structure.  Should it find an inode with a LUT in need of
 * adjustment it allocates memory for the new LUT, calls lut_fill() to fill
 * it in and, once this completes, replaces the existing LUT with the new
 * one.  Note that lut_fill() ripples this change down the tree by recursive
 * calls through lut_want() and back to lut_fill() again; with the same-byte
 * constraint and the current optimization schedules the maximum recursion
 * depth will be 3.  Should any of these newly constructed LUTs cover or
 * (de)reference the target inode the target bit is reset.  If the target
 * bit is still set on the inode after all the LUT changes, if any, are
 * completed lut_optimize() corrects the reference in the existing LUT and
 * turns the target bit off itself.  Finally, the act of rearranging the LUTs
 * may have caused inodes which formerly had LUTs themselves to be covered,
 * making the LUTs "useless".  Useless LUTs are counted but not removed when
 * found to avoid making the search structure transiently more expensive for
 * concurrent lookups, so if the useless LUT count is non-zero when the LUT
 * changes are complete lut_cleanup() is called to find and free them.
 *
 * Note that most of what lut_optimize() does has no effect on the correctness
 * of the LUT data structure and searches done using it.  The only thing it
 * must do is whatever adjustment is necessary to allow the target bit on the
 * inode to be reset; if the target bit is not set when lut_optimize() is
 * called then lut_optimize() can safely do nothing at all.  This means that
 * if an LUT memory allocation fails lut_optimize() can always respond by
 * leaving things just the way they are as long as it does what it needs to
 * to reset the target bit (which minimally requires no memory allocation, just
 * a pointer rewrite in an existing LUT) before returning, leaving the LUT
 * structure correct but perhaps suboptimal.  Since LUTs typically only need
 * changing when the number of inodes changes this implies that we only expect
 * lut_optimize() to do anything at all when the target bit is set on its
 * inode (i.e. often for adds and deletes, but never for changes) but we call
 * lut_optimize() in all cases anyway since it is fairly cheap for
 * lut_optimize() to determine nothing needs to be done itself and this allows
 * it to correct suboptimalities caused by prior LUT allocation failures or,
 * perhaps, a change to the optimization schedule made at other than tree
 * initialization time.
 *
 * On return from rttree_update_() to its caller the LUTs and attached
 * parent pointers will be fully updated to match the change the caller
 * made to the basic structure.  If the caller is rttree_delete(), however,
 * the change to the basic structure may be incomplete since a one-way
 * branching inode (with its target and delete bits set when rttree_update_()
 * was called) will still be present.  With all references to the inode having
 * been removed from the LUTs rttree_delete() can complete its work by
 * stitching the inode out of the basic structure as well.
 *
 * While we noted the rule that we should size each LUT based on the number
 * of single bit tests (i.e. inodes) a LUT of that size eliminates to maintain
 * O(n) memory consumption, we neglected to discuss the size those numbers
 * should be.  It is clear that an m-bit LUT will maximally cover (2^m - 2)
 * inodes, so the number where we consider adding a LUT of that size should
 * probably be a fraction of that, but how small that fraction should be is
 * the question.  The answer is probably that there is no correct answer, or
 * at least that I haven't looked at this hard enough to argue for one, but
 * rather a direct tradeoff between per-external-node memory consumption and
 * lookup performance which we deal with by making the optimization schedule
 * programmable so someone else can decide.  Search for rttree_opt_ below
 * for some examples.  Note that a schedule has two numbers for each LUT
 * size, the high water mark at which we consider growing a LUT and the low
 * water mark at which we consider shrinking it.  Making the latter number
 * a bit smaller than the former adds some hysteresis, reducing the cases
 * where a single route going in and out of the table (a not infrequent
 * occurance in large Internet forwarding tables) causes LUTs to be
 * frequently built and rebuilt.  If forced to express an opinion about
 * the two schedules below I'd say the "timid" schedule is way too timid,
 * even though it is the default for hysterical reasons (it matches the
 * memory consumption of the traditional BSD radix trie, even though the
 * latter was invented at a time when memory was much dearer than now),
 * while the "aggressive" schedule could probably afford to be even more
 * aggressive since it still leaves the fraction of the total table
 * memory consumption eaten by LUTs at a fairly modest value when the
 * likely size of external nodes is included in the accounting.  Different
 * schedules can be invented and put to use by calling rttree_tune().
 *
 * You may remember above that we characterized the cost of rttree_delete()
 * and rttree_change() (and rttree_add()) as O(log(n)) because of the cost of
 * the searches they would need to do in the basic structure.  Since those
 * functions as implemented instead do searches in the LUT structure their
 * scaling is in fact improved to that of a LUT search.  rttree_add() also
 * uses the LUT structures for a downward search but makes use of the basic
 * structure for other bits of its operation, so it may remain O(log(n)).
 * This, of course, ignores the cost of the call to rttree_update_(),
 * the scaling behaviour of which I'm not sure how to characterize.
 * Occasionally rttree_update_() may be called upon to do a rather
 * daunting amount of work (O(n) when it needs to rewrite a lot of attached
 * parent pointers) but its normal case is that it rewrites a single
 * pointer in a LUT and then returns.  Testing of _add() and _delete()
 * modifications in very large tables showed most of the work being spent
 * reading and parsing (very inefficiently) the ASCII list of IP addresses
 * to operate on so I suspect things like system call and memory allocation
 * overhead might have a greater impact on the rate at which a kernel route
 * table might be changed than the actual maintenance of the data structure.
 *
 * Finally, there is the issue of why we accept the cost of maintaining
 * both the basic 4-pointer inode structure and the LUT+attached parent
 * structure when we only use the latter for searches?  It does seem like
 * we could get a better result if we took the memory that would be saved
 * by eliminating the basic structure altogether (e.g. all the covered
 * inodes that aren't used for searches) and spent that memory on an
 * even more aggressive LUT structure, and there is truth in the observation
 * that the basic structure constrains the LUT structure in ways that may
 * make the latter less optimal (you are constrained to place LUTs only
 * in spots where the basic structure has placed inodes, while the LUT
 * structure by itself might be improved by being less constrained about
 * LUT placement).
 *
 * I would point out that while the LUT structure is most useful for tree
 * searches, the basic structure seems to be most useful for lexical tree walk
 * operations.  A _next() or _prev() operation in the basic structure is O(1)
 * on average and fairly direct while in the LUTs the many NULL pointers,
 * which are a benefit to searches, become an added cost for lexical walks.
 * While it is true that for many application uses of the structure will
 * value searches far more highly than tree walks, it is also the case that
 * rttree_update_() implementation makes very extensive use of the efficiency
 * of tree walks in the basic structure as a replacement for deep recursion
 * (the only recursive implementation remaining is the loop through lut_fill()
 * and lut_want(), which is depth-limited).  It is also the existence of the
 * basic structure backstop that allows LUT allocations to be best effort,
 * which in turn allows us the luxury of not having to back out of an LUT
 * rearrangement requiring a significant number of separate allocations when
 * the last of these fails (this might not be convincing if memory allocation
 * is reliable, but it still gives us the luxury of not having to ensure that).
 * And having the two structures seems to subdivide the maintenance problem
 * in a quite useful way.  The _add(), _delete() and _change() implementations
 * can compute the effect of a modification on the basic structure, where
 * things are relatively simple and incremental, while mostly ignoring the
 * LUT/attached parent version of the structure.  rttree_update_() deals
 * with the latter but has the advantage of the change being fully
 * reflected in the basic structure by the time it sees it, making the
 * work of updating the LUT structures to match and then rearranging them,
 * a problem of copying stuff out of the basic structure into the corresponding
 * places into the LUT structures.
 *
 * Of course what this is really saying is that I know how to build the LUT
 * structure on the scaffold of the basic structure but don't know how to do
 * it without that.  I believe there is room for improvement here if a
 * better approach can be invented so I'm hoping the long blurb above, in
 * addition to providing background to understand what is going on below,
 * might inspire some thinking about what could be done instead.
 */


/*
 * Coding of our lookup table size allocations is a bit strange.
 * The decoding key for the number of pointers in an lut is:
 *
 *     0    1    2    3    4    5    6    7    8
 *    256  128  64   32   16    8    4    2    1
 *
 * 8 is actually a pseudo-value, there are no 1-way lookup
 * tables.
 *
 * We compute the index for a LUT attached to an inode from the
 * key word (the word index is the inode bit number shifted right
 * 5 bits) by shifting the word left by the low order 5 bits of the
 * inode bit number, adjusted for endianness, and then shifting the word
 * right by the LUT size as encoded above plus 24.  
 *
 * Redefined RTT_LUT_x to LUT_x for x=1,2,256 to save typing below.
 */
#define	LUT_256		RTT_LUT_256
#define	LUT_2		RTT_LUT_2
#define	LUT_1		RTT_LUT_1	/* a pseudo value */

/*
 * LUT optimization arrays.  These are the built-in options; the
 * library user may make up his own and call rttree_tune() to put
 * these into use for a particular tree.
 *
 * Each array entry is matched against the total number of inodes
 * in the `n' ranks below us, where n=7,6,5,4,3,2,1.  The .hi value
 * is the high watermark, we consider adding an LUT of that size
 * if the number of inodes covered by an LUT of that size is at
 * least that large.  Once we added an LUT we consider deleting
 * it if the number of inodes drops below the second number, the .lo
 * value or the low watermark.  The low watermark must be no larger
 * than the high watermark, and should probably be somewhat less so
 * a single route going in and out doesn't cause us to do a bunch
 * of work adding and deleting the same LUT on every change.
 *
 * Note that 4-way branches are probably a bad idea; the 3 value
 * as the high water mark prevents us from having any.  The reason
 * for this is that if you measure the work done in a lookup by
 * cache lines read a 4-way doesn't save anything at all, but
 * allowing 4-ways does dramatically increase the rate at which
 * we allocate and delete LUTs so they generally make changes most
 * costly for no gain in performance (measured by cache lines).
 *
 * We define two built-in optimization schedules at this point.
 * The rttree_opt_timid[] schedule adds an LUT when it will be about
 * (not quite) half full of pointers pointing down the tree; in
 * the worst case the LUTs will consume about a quarter of the memory
 * that the inodes do (assuming inodes are about 8 pointers in size).
 * The rttree_opt_aggressive[] schedule is instead chosen so that the
 * worst case memory consumption by LUTs will be about the same as
 * the inode allocation; that is the LUTs will about double the
 * amount of memory spent on internal data structure in the worst
 * case.  In general the real-life normal memory consumption by LUTs
 * will be about half the worst case.  At the time of writing rttree_init()
 * uses rttree_opt_timid by default since it makes the total per-external
 * overhead of this structure more-or-less match the old radix tree in the
 * kernel.  rttree_opt_aggressive works nice, though.
 */
const rtt_opt_t rttree_opt_timid[RTT_LUT_NUM] = {
	{ 128, 96 },	/* of 254; 256-way */
	{ 64, 48 },	/* of 126; 128-way */
	{ 32, 24 },	/* of 62; 64-way */
	{ 16, 12 },	/* of 30; 32-way */
	{ 7, 4 },	/* of 14; 16-way */
	{ 3, 1 },	/* of 6; 8-way */
	{ 3, 1 }	/* of 2; 4-way */
};

const rtt_opt_t rttree_opt_aggressive[RTT_LUT_NUM] = {
	{ 33, 27 },	/* of 254; 256-way */
	{ 17, 14 },	/* of 126; 128-way */
	{ 9, 7 },	/* of 62; 64-way */
	{ 5, 3 },	/* of 30; 32-way */
	{ 3, 2 },	/* of 14; 16-way */
	{ 2, 1 },	/* of 6; 8-way */
	{ 3, 1 }	/* of 2; 4-way */
};


/*
 * Inode counts are kept in the inode member rtti_counts[5]
 * array.  This counts inodes below the target, indexed by
 * the low order 3 bits of the inode bit number within the
 * same key byte.  By counting inodes by their bit number a
 * new inode can initialize its own counts by adding up the
 * counts of its two inode children (if they test bits in the
 * same key byte) and then incrementing the sum by 1 for each
 * child using the low order three bits of the child's bit number.
 * See cnt_add() below.  There is hence no need for a new inode to
 * actually look at all the inodes below it to initialize its counts,
 * it just needs to look at its own direct children.  When an inode
 * is deleted it can correct the counts of the inodes above by
 * decrementing the count in each indicated by the low order 3 bits
 * of its own bit number.
 *
 * Note that this count definition (i.e. the count of inodes below
 * us with a bit number having a particular 3 low order bits; each
 * inode below increments only one count) is different then the
 * cumulative number of inodes in the `n' ranks below us that is
 * the measure used by the optimization schedules above.  The former
 * arrangement is easier to maintain when the tree changes while
 * the latter is more appropriate for the task of optimization.
 * This means that when we need the optimization counts for comparison
 * with the schedule we need to compute them as a cumulative sum of the
 * counts in the inode (done in cnt_extract() below).
 *
 * The problem with this is that there can be up to 7 counts to keep
 * (below an inode with a bit number whose low order 3 bits are zero),
 * but rtti_counts[] only has 5 bytes to keep them in.  Furthermore,
 * we are so strapped for space in an inode that we need to use some of
 * those bits for unrelated flags.
 *
 * The following describes the layout for the counts.  The
 * first array is indexed by the rtti_count[] index and masks
 * the bits in that byte which are counts (instead of flags) to
 * use when we are summing the counts in a pair of child inodes.
 * The second is indexed by the low order 3 bits of a bit number
 * and describes where the count for nodes in that bit rank is
 * stored.
 */
static const uint8_t cnt_masks[5] = {
	0xff,		/* bit rank 7, max 128 */
	0x7f,		/* bit rank 6, max 64, 1 bit for flag */
	0x3f,		/* bit rank 5, max 32, 2 bits for flags */
	0xff,		/* bit ranks 4 & 2, max 16 & 4 */
	0x3f		/* bit ranks 3 & 1, max 8 & 2, 2 bits for flags */
};

/*
 * The count description array is a structure.
 */
typedef struct cnt_desc_ {
	uint8_t		cd_idx;		/* index to byte with this count */
	uint8_t		cd_shift;	/* shift to isolate count */
	uint8_t		cd_mask;	/* mask to isolate count */
	uint8_t		cd_inc;		/* value to add to increment count */
} cnt_desc_t;

static const cnt_desc_t cnt_descs[RTT_LUT_SIZES] = {
	{ 0,	0,	0x00,	0x00 },	/* invalid rank */
	{ 4,	4,	0x30,	0x10 },	/* bit rank 1, max 2 */
	{ 3,	5,	0xe0,	0x20 },	/* bit rank 2, max 4 */
	{ 4,	0,	0x0f,	0x01 },	/* bit rank 3, max 8 */
	{ 3,	0,	0x1f,	0x01 },	/* bit rank 4, max 16 */
	{ 2,	0,	0x3f,	0x01 },	/* bit rank 5, max 32 */
	{ 1,	0,	0x7f,	0x01 },	/* bit rank 6, max 64 */
	{ 0,	0,	0xff,	0x01 },	/* bit rank 7, max 128 */
};

/*
 * Here are the bits we use (to keep it close to the above).
 * We have a `covered' bit, which indicates whether the inode
 * is covered by an LUT above, and an LUT bit, which indicates
 * that this inode has an LUT allocated to it.  We keep them
 * both in the same byte since, if they are both set, we call
 * the inode (or its LUT) `useless' since the LUT is unused.
 *
 * We also have a `deleted' bit to indicate that the inode is
 * in the process of being deleted from the tree.
 */
#if 0
/* These moved to rttree_update.h so rttree_init() can see them */
#define	CNT_HAS_LUT	4
#define	RI_HAS_LUT	0x40	/* the inode has a non-trivial LUT of its own */
#endif	/* 0 */
#define	CNT_USELESS	CNT_HAS_LUT	/* count we store `covered' and `lut' */
#define	RI_COVERED	0x80	/* the inode is `covered' by an LUT above */
#define	RI_USELESS	(RI_COVERED|RI_HAS_LUT)	/* the LUT is useless */
#define	rtti_useless	rtti_counts[CNT_USELESS]
#define	rtti_covered	rtti_counts[CNT_USELESS]
#define	rtti_has_lut	rtti_counts[CNT_USELESS]

#define	rtti_deleted	rtti_counts[CNT_TARGET]
#define	rtti_target	rtti_counts[CNT_TARGET]


/*
 * Inline which, given a bit number (of the inode to which you
 * are considering attaching an LUT), returns a pointer to
 * the relevant part of root's rttr_opt[] array.
 */
static inline const rtt_opt_t *
lut_get_wat(const rttree_t *, rtt_ibit_t) ALWAYS_INLINE;

static inline const rtt_opt_t *
lut_get_wat (const rttree_t *rr, rtt_ibit_t bit)
{

	return (&rr->rttr_opt[RN_BIT(bit)]);
} 


/*
 * Inline which, given pointer to watermark values returned above
 * and a child bit number, returns the appropriate high watermark
 * for an LUT just covering that child.
 */
static inline unsigned int
lut_hiwat(const rtt_opt_t *, rtt_ibit_t) ALWAYS_INLINE;

static inline unsigned int
lut_hiwat (const rtt_opt_t *wat, rtt_ibit_t bit)
{

	return (wat[RN_BIT(~bit)].hi);
}


/*
 * Similar to the above, but returns a low watermark value.
 */
static inline unsigned int
lut_lowat(const rtt_opt_t *, rtt_ibit_t) ALWAYS_INLINE;

static inline unsigned int
lut_lowat (const rtt_opt_t *wat, rtt_ibit_t bit)
{

	return (wat[RN_BIT(~bit)].lo);
}


/*
 * Routines to test and manipulate the rtt_inode_t flags.
 */

/*
 * ri_is_covered()
 *
 * Return true if the rtt_inode_t has the covered bit set.
 */
static inline bool ri_is_covered(const rtt_inode_t *) ALWAYS_INLINE;

static inline bool
ri_is_covered (const rtt_inode_t *ri)
{

	return ((ri->rtti_covered & RI_COVERED) != 0);
}


/*
 * ri_set_covered()
 *
 * Set/clear the covered bit in an rtt_inode_t.
 */
static inline void ri_set_covered(rtt_inode_t *, bool) ALWAYS_INLINE;

static inline void
ri_set_covered (rtt_inode_t *ri, bool covered)
{

	if (covered) {
		ri->rtti_covered |= RI_COVERED;
	} else {
		ri->rtti_covered &= ~RI_COVERED;
	}
}


/*
 * ri_has_lut()
 *
 * Return true if the rtt_inode_t has the has_lut bit set.
 */
static inline bool ri_has_lut(rtt_inode_t *) ALWAYS_INLINE;

static inline bool
ri_has_lut (rtt_inode_t *ri)
{

	return ((ri->rtti_has_lut & RI_HAS_LUT) != 0);
}


/*
 * ri_set_has_lut()
 *
 * Set/clear the has_lut bit in an rtt_inode_t.
 */
static inline void ri_set_has_lut(rtt_inode_t *, bool) ALWAYS_INLINE;

static inline void
ri_set_has_lut (rtt_inode_t *ri, bool has_lut)
{

	if (has_lut) {
		ri->rtti_has_lut |= RI_HAS_LUT;
	} else {
		ri->rtti_has_lut &= ~RI_HAS_LUT;
	}
}


/*
 * ri_is_useless()
 *
 * Return true if the rtt_inode_t has a useless LUT.
 */
static inline bool ri_is_useless(rtt_inode_t *) ALWAYS_INLINE;

static inline bool
ri_is_useless (rtt_inode_t *ri)
{

	return ((ri->rtti_useless & RI_USELESS) == RI_USELESS);
}

/*
 * ri_is_lut_covered()
 *
 * Return true if the rtt_inode_t either has an LUT or is covered
 * by one from above.
 */
static inline bool ri_is_lut_covered(rtt_inode_t *) ALWAYS_INLINE;

static inline bool
ri_is_lut_covered (rtt_inode_t *ri)
{

	return ((ri->rtti_useless & (RI_HAS_LUT|RI_COVERED)) != 0);
}


/*
 * ri_is_deleted()
 *
 * Return true if the rtt_inode_t has the deleted bit set.
 */
static inline bool ri_is_deleted(rtt_inode_t *) ALWAYS_INLINE;

static inline bool
ri_is_deleted (rtt_inode_t *ri)
{

	return ((ri->rtti_deleted & RI_DELETED) != 0);
}



#ifdef notdef
/*
 * ri_set_deleted()
 *
 * Set/clear the deleted flag in an rtt_inode_t.
 */
static inline void ri_set_deleted(rtt_inode_t *, bool) ALWAYS_INLINE;

static inline void
ri_set_deleted (rtt_inode_t *ri, bool deleted)
{

	if (deleted) {
		ri->rtti_deleted |= RI_DELETED;
	} else {
		ri->rtti_deleted &= ~RI_DELETED;
	}
}
#endif	/* notdef */


/*
 * ri_is_target()
 *
 * Return true if the rtt_inode_t has the target bit set.
 */
static inline bool ri_is_target(rtt_inode_t *) ALWAYS_INLINE;

static inline bool
ri_is_target (rtt_inode_t *ri)
{

	return ((ri->rtti_deleted & RI_TARGET) != 0);
}



/*
 * ri_set_target()
 *
 * Set/clear the deleted flag in an rtt_inode_t.
 */
static inline void ri_set_target(rtt_inode_t *, bool) ALWAYS_INLINE;

static inline void
ri_set_target (rtt_inode_t *ri, bool target)
{

	if (target) {
		ri->rtti_target |= RI_TARGET;
	} else {
		ri->rtti_target &= ~RI_TARGET;
	}
}


/*
 * ri_was_seen()
 *
 * When operating on optimization structures we clear the target
 * bit on any inode where we find it set so that the caller
 * can know that this inode has been dealt with.  This optimizes
 * that frequent operation.
 */
static inline void
ri_was_seen (rtt_inode_t *ri)
{
	uint8_t v;

	v = ri->rtti_target;
	if ((v & RI_TARGET) != 0) {
		ri->rtti_target = (v & ~RI_TARGET);
	}
}



/*
 * The inode count functions are next.  Each inode
 * keeps a count of the number of inodes and child
 * rnodes in the 1-7 ranks below it up to the next byte
 * boundary.  The counts are indexed by the low order 3 bits
 * of the bit number of the inode being counted, giving a
 * value of 1-7.  The cnt_descs[] array tells us what
 * to do.
 */

#if 0
/*
 * cnt_get()
 *
 * Fetch the inode count for the specified rank.
 */
static inline unsigned int cnt_get(rtt_inode_t *, rtt_ibit_t) ALWAYS_INLINE;

static inline unsigned int
cnt_get (rtt_inode_t *ri, rtt_ibit_t b)
{
	unsigned int count;
	const cnt_desc_t *cd;

	cd = &cnt_descs[RN_BIT(b)];
	count = (ri->rtti_counts[cd->cd_idx] & cd->cd_mask) >> cd->cd_shift;
	return (count);
}
#endif	/* 0 */


/*
 * cnt_inc_dec()
 *
 * Add/subtract 1 to/from an inode count
 */
static inline void cnt_inc_dec(rtt_inode_t *, rtt_ibit_t, bool) ALWAYS_INLINE;

static inline void
cnt_inc_dec (rtt_inode_t *ri, rtt_ibit_t b, bool decrement)
{
	const cnt_desc_t *cd;
	unsigned int idx;
	uint8_t v;

	cd = &cnt_descs[RN_BIT(b)];
	idx = cd->cd_idx;
	v = ri->rtti_counts[idx];
	if (decrement) {
		v -= cd->cd_inc;
	} else {
		v += cd->cd_inc;
	}
	ri->rtti_counts[idx] = v;
}

/*
 * cnt_extract()
 *
 * Return the cumulative counts used for LUT sizing in an
 * array indexed by bit number.  RI_EXT() is a utility to
 * save typing.
 */
#define	RI_EXT(cnt, i) \
	(((cnt)[cnt_descs[(i)].cd_idx] & cnt_descs[(i)].cd_mask) >> \
		cnt_descs[(i)].cd_shift)

static inline void
cnt_extract(rtt_inode_t *, rtt_ibit_t, uint8_t *) ALWAYS_INLINE;

static inline void
cnt_extract(rtt_inode_t *ri, rtt_ibit_t bit, uint8_t *cnts)
{
	unsigned int i, b;
	const uint8_t *ri_cnts;

	b = RN_BIT(bit);
	for (i = 0; i <= b; i++) {
		cnts[i] = 0;
	}

	ri_cnts = ri->rtti_counts;
	switch (b) {
	case 0:
		cnts[1] = RI_EXT(ri_cnts, 1);
		/*FALLSTHROUGH*/

	case 1:
		cnts[2] = RI_EXT(ri_cnts, 2) + cnts[1];
		/*FALLSTHROUGH*/

	case 2:
		cnts[3] = RI_EXT(ri_cnts, 3) + cnts[2];
		/*FALLSTHROUGH*/

	case 3:
		cnts[4] = RI_EXT(ri_cnts, 4) + cnts[3];
		/*FALLSTHROUGH*/

	case 4:
		cnts[5] = RI_EXT(ri_cnts, 5) + cnts[4];
		/*FALLSTHROUGH*/

	case 5:
		cnts[6] = RI_EXT(ri_cnts, 6) + cnts[5];
		/*FALLSTHROUGH*/

	case 6:
		cnts[7] = RI_EXT(ri_cnts, 7) + cnts[6];
		/*FALLSTHROUGH*/

	default:
		break;
	}
}


/*
 * cnt_add()
 *
 * Add the second inode, and its counts, to the first inode's
 * counts.  This is called when the counts in the first inode
 * are being initalized from the counts of its children.
 */
static void cnt_add(rtt_inode_t *, rtt_inode_t *) ALWAYS_INLINE;

static void
cnt_add (rtt_inode_t *ri, rtt_inode_t *a_ri)
{
	rtt_ibit_t bit;

	/*
	 * Add in the inode itself.
	 */
	bit = a_ri->rtti_bit;
	if (!rn_same_byte(ri->rtti_bit, bit)) {
		return;
	}
	bit = RN_BIT(bit);
	cnt_inc_dec(ri, bit, false);

	/*
	 * Now add in the counts.  This has an unseemly
	 * knowledge of which bytes have which counts.  Note
	 * that a_ri will only have non-zero rank counts
	 * at ranks beyond his bit number's rank.
	 */
	switch (bit) {
	case 0:		/* can't happen */
	case 1:
	case 2:
		/* ranks 1, 2 and 3 need to add all 5 bytes */
		ri->rtti_counts[4] += a_ri->rtti_counts[4] & cnt_masks[4];
		/*FALLSTHROUGH*/

	case 3:
		/* rank 4 can skip the last byte */
		ri->rtti_counts[3] += a_ri->rtti_counts[3] & cnt_masks[3];
		/*FALLSTHROUGH*/

	case 4:
		/* rank 5 can skip the last 2 bytes */
		ri->rtti_counts[2] += a_ri->rtti_counts[2] & cnt_masks[2];
		/*FALLSTHROUGH*/

	case 5:
		/* rank 6 counts only the first 2 bytes */
		ri->rtti_counts[1] += a_ri->rtti_counts[1] & cnt_masks[1];
		/*FALLSTHROUGH*/

	case 6:
		/* rank 7 counts only the first byte */
		ri->rtti_counts[0] += a_ri->rtti_counts[0] & cnt_masks[0];
		/*FALLSTHROUGH*/

	default:
		/* come here when a_ri's bit is rank 7.  Nothing to count */
		break;
	}
}


/*
 * ri_set_byte()
 *
 * Given an inode and a bit number (probably from the inode's
 * parent), set the inode byte.
 */
static void
ri_set_byte (rttree_t *rr, rtt_inode_t *ri, rtt_ibit_t p_bit)
{
	rtt_ibit_t bit;
	rtt_inode_t *c_ri;
	rtt_ptr_t rptr;
	rtnode_t *rn;
	uint8_t byte;

	/*
	 * Find a child external to extract the byte from.
	 */
	c_ri = ri;
	while ((rptr = c_ri->rtti_attached) == RP_NULL) {
		if (rp_is_ext(rptr = c_ri->rtti_right)) {
			break;
		}
		if (rp_is_ext(rptr = c_ri->rtti_left)) {
			break;
		}
		c_ri = rp_to_int(rptr);
	}
	rn = rp_to_ext(rptr);
	byte = rn_key_byte(rn_key(rr, rn), p_bit);

	/*
	 * Get his bit.  If it is in the same byte as the
	 * parent bit we will need to zero some low order bits.
	 */
	bit = ri->rtti_bit;
	if (rn_same_byte(bit, p_bit)) {
		byte &= 0xff << RN_BIT(-bit);
	}
	ri->rtti_byte = byte;
}


/*
 * Internal structure walk routines are next
 */

/*
 * ri_next()
 *
 * Internal function to find the next inode in a subtree rooted
 * at an inode with the specified bit number.  The inode is found by
 * searching down the tree, left side first, returning each inode
 * we find, until an inode is encountered with no inode children.
 * At that point we search back up the tree looking for a right side
 * inode which wasn't one we visited, or until we reach an inode with a
 * bit number equal to (or less than) the root bit number.
 *
 * Setting the `prune' argument to true causes it to skip searching
 * below the current inode, in effect pruning that sub-subtree.
 */
static inline rtt_inode_t *
ri_next (rtt_inode_t *curr_ri, rtt_ibit_t bit, bool prune)
{
	rtt_inode_t *ri;
	rtt_ptr_t ptr;
	rtt_ibit_t ri_bit;

	if (!prune) {
		ptr = curr_ri->rtti_left;
		if (rp_is_int(ptr)) {
			return (rp_to_int(ptr));
		}
		ptr = curr_ri->rtti_right;
		if (rp_is_int(ptr)) {
			return (rp_to_int(ptr));
		}
	}

	if (curr_ri->rtti_bit <= bit) {
		return (NULL);
	}
	ri = curr_ri->rtti_parent;
	while ((ri_bit = ri->rtti_bit) >= bit) {
		ptr = ri->rtti_right;
		if (rp_is_int(ptr) && rp_to_int(ptr) != curr_ri) {
			return (rp_to_int(ptr));
		}
		if (ri_bit == bit) {
			break;
		}
		curr_ri = ri;
		ri = ri->rtti_parent;
	}

	return (NULL);
}


/*
 * ri_next_max()
 *
 * Like ri_next(), but only returns inodes which test bits between
 * 'bit' and 'max_bit'.
 */
static inline rtt_inode_t *
ri_next_max (rtt_inode_t *c_ri, rtt_ibit_t bit, rtt_ibit_t max_bit, bool prune)
{
	rtt_inode_t *ri;
	rtt_ptr_t ptr;
	rtt_ibit_t ri_bit;

	if (!prune && c_ri->rtti_bit < max_bit) {
		ptr = c_ri->rtti_left;
		if (rp_is_int(ptr)) {
			ri = rp_to_int(ptr);
			if (ri->rtti_bit <= max_bit) {
				return (ri);
			}
		}
		ptr = c_ri->rtti_right;
		if (rp_is_int(ptr)) {
			ri = rp_to_int(ptr);
			if (ri->rtti_bit <= max_bit) {
				return (ri);
			}
		}
	}

	/*
	 * Make sure c_ri is somewhere down the tree
	 */
	if (c_ri->rtti_bit <= bit) {
		return (NULL);
	}

	ri = c_ri->rtti_parent;
	while ((ri_bit = ri->rtti_bit) >= bit) {
		rtt_inode_t *n_ri;

		ptr = ri->rtti_right;
		if (rp_is_int(ptr) && (n_ri = rp_to_int(ptr)) != c_ri &&
		    n_ri->rtti_bit <= max_bit) {
			return (n_ri);
		}
		if (ri->rtti_bit == bit) {
			break;
		}
		c_ri = ri;
		ri = c_ri->rtti_parent;
	}

	return (NULL);
}



/*
 * Internal functions to manipulate lookup tables are next.
 */

/*
 * lut_to_size()
 *
 * Given a bit number where a LUT resides, and the maximum
 * bit number it covers, return an rtt_size_t size.
 */
static inline rtt_size_t
lut_to_size (rtt_ibit_t bit, rtt_ibit_t max_bit)
{

	return ((rtt_size_t) RI_LUT(~(max_bit - bit)));
}

/*
 * lut_2way()
 *
 * Given an inode, make an RTT_LUT_2 lookup table pointer
 * pointing to the internal pointer pair.
 */
static inline rtt_lut_t
lut_2way (rtt_inode_t *ri)
{

	return (lut_make(ri->rtti_child, LUT_2));
}

/*
 * lut_seg_index()
 *
 * Given the bit number an LUT is attached to, the bit
 * number the LUT extends to and a key byte, return
 * an index into the LUT for the byte.
 */
static inline unsigned int
lut_seg_index(rtt_ibit_t, rtt_ibit_t, unsigned int) ALWAYS_INLINE;

static inline unsigned int
lut_seg_index (rtt_ibit_t lut_bit, rtt_ibit_t lut_maxbit, unsigned int byte)
{

	/*
	 * Clear the high order bits we don't want based
	 * on lut_bit.  Shift it over to eliminate the low
	 * order bits we don't care about using lut_maxbit.
	 */
	byte &= 0xff >> RN_BIT(lut_bit);
	byte >>= RN_BIT(~lut_maxbit);
	return (byte);
}


/*
 * lut_seg_length()
 *
 * The second half of the above. Given the bit number an LUT is attached
 * to, the maximum bit number the LUT covers and a bit associated with
 * something to be listed in the LUT, determine the length of the
 * segment covered.
 */
static inline unsigned int
lut_seg_length(rtt_ibit_t, rtt_ibit_t, rtt_ibit_t) ALWAYS_INLINE;

static inline unsigned int
lut_seg_length (rtt_ibit_t lut_bit, rtt_ibit_t lut_maxbit, rtt_ibit_t bit)
{

	if (bit < lut_bit) {
		bit = lut_bit;
	} else if (bit > lut_maxbit) {
		return (1);
	}

	return (2u << (lut_maxbit - bit));
}


/*
 * lut_seg_end_index()
 *
 * The third half of the above, I guess.  Given the low and high
 * coverage bits of an LUT, the byte from an inode or key and
 * the bit tested by the inode of that byte, return the end
 * index of the segment.
 */
static inline unsigned int lut_seg_end_index(rtt_ibit_t, rtt_ibit_t,
				rtt_ibit_t, unsigned int) ALWAYS_INLINE;

static inline unsigned int
lut_seg_end_index (rtt_ibit_t lut_bit, rtt_ibit_t lut_maxbit,
		   rtt_ibit_t bit, unsigned int byte)
{
	unsigned int i = 0;

	if (bit > lut_bit) {
		i = lut_seg_index(lut_bit, lut_maxbit, byte);
	}
	return (i + lut_seg_length(lut_bit, lut_maxbit, bit));
}


#if 0
/*
 * lut_length()
 *
 * Return the full length of an LUT, in entries, given the bit number
 * of the node it is attached to and the maximum bit number it covers.
 */
static inline unsigned int lut_length(rtt_ibit_t, rtt_ibit_t) ALWAYS_INLINE;

static inline unsigned int
lut_length (rtt_ibit_t lut_bit, rtt_ibit_t lut_maxbit)
{

	return (2u << (lut_maxbit - lut_bit));
}
#endif	/* 0 */


/*
 * lut_newsize()
 *
 * Given an inode, the inode's bit number (to avoid re-reading it) and
 * the size of an LUT it has (or should be assumed to have), determine
 * the size of an LUT that it should have.  Returns an rtt_size_t.
 */
static inline rtt_size_t
lut_newsize (rttree_t *rr, rtt_inode_t *ri, rtt_ibit_t bit, rtt_size_t o_size)
{
	rtt_ibit_t max_bit, hi_bit, lo_bit;
	const rtt_opt_t *wat;
	uint8_t cnts[RTT_LUT_SIZES];
	unsigned int c_fill;

	/*
	 * Set bit to the bit number in the inode.  Compute
	 * maxbit based on the size offered.
	 */
	hi_bit = RN_MAXBIT(bit);
	max_bit = bit + lut_isize(o_size);
	if (max_bit > hi_bit) {
		max_bit = bit;		/* sanity */
	}

	/*
	 * Assume we'll try every size.  If this is the root
	 * inode, however, we'll catch the special case.
	 */
	if (bit == 0) {
		rtt_size_t root_size = rr->rttr_root_lut;

		if (root_size == LUT_256) {
			return (LUT_256);	/* can't change size */
		}
		lo_bit = lut_isize(root_size);
	} else {
		lo_bit = bit; 
	}

	/*
	 * Determine if we're trying to grow or shrink it.  This
	 * will tell us the range of bit numbers to check.
	 */
	wat = lut_get_wat(rr, bit);
	cnt_extract(ri, bit, cnts);
	if (max_bit > bit) {
		/*
		 * Existing LUT.  Shrink it if it is below
		 * the low water mark, expand it otherwise.
		 */
		c_fill = cnts[RN_BIT(max_bit)];
		if (c_fill < lut_lowat(wat, max_bit)) {
			/* Shrink it here */
			hi_bit = max_bit - 1;
		} else if (lo_bit < max_bit) {
			/* Look at bigger sizes only */
			lo_bit = max_bit;
		}
	}

	/*
	 * See if we're above the high watermark at any size
	 * between (lo_bit + 1) and hi_bit, inclusive.  Start
	 * at the big end and work back.
	 */
	if (hi_bit <= lo_bit) {
		hi_bit = lo_bit;
	} else {
		while (hi_bit > lo_bit) {
			c_fill = cnts[RN_BIT(hi_bit)];
			if (c_fill >= lut_hiwat(wat, hi_bit)) {
				break;
			}
			hi_bit--;
		}
	}

	/*
	 * Got whatever we found in hi_bit.  Return it.
	 */
	return (lut_to_size(bit, hi_bit));
}


/*
 * lut_uncover()
 *
 * Given an inode and a bit range, clear the covered bit on
 * all inodes in the subtree in that bit range.
 */
static inline void
lut_uncover (rtt_inode_t *ri, rtt_ibit_t min_bit, rtt_ibit_t max_bit)
{

	do {
		ri_set_covered(ri, false);
	} while ((ri = ri_next_max(ri, min_bit, max_bit, false)) != NULL);
}


/*
 * lut_cleanup()
 *
 * This is called when a rearrangement of the LUTs in this area of
 * the tree has been completed.  The argument inode is the inode highest
 * in the tree where something changed.  We walk below there looking
 * for "useless" LUTs, those that got covered by the structure just
 * added, and we remove and free them.
 */
static void
lut_cleanup (rttree_t *rr, rtt_inode_t *p_ri, int n_useless)
{
	rtt_inode_t *ri;
	rtt_ibit_t bit, max_bit;
	rtt_lut_t lut;
	int n_found;

	/*
	 * Count up what we find.  If we trust n_useless we
	 * can quit when we find that many.  Inodes with the
	 * RN_MAXBIT() bit number never have LUTs added so
	 * we only need to search to one bit short of that.
	 */
	n_found = 0;
	ri = p_ri;
	bit = ri->rtti_bit;
	max_bit = RN_MAXBIT(bit) - 1;
	while ((ri = ri_next_max(ri, bit, max_bit, false)) != NULL) {
		/*
		 * Don't remove the lut from a deleted node.  The
		 * caller will eventually dump it and it wasn't
		 * counted in n_useless so the check below will
		 * terminate us too early.
		 */
		if (!ri_is_useless(ri) || ri_is_deleted(ri)) {
			continue;
		}

		/*
		 * Get the old lookup table, then rewrite it
		 * with an LUT_2 pointing at the node itself.
		 * Also clear off the has_lut flag.
		 */
		lut = ri->rtti_lut;
		ri->rtti_lut = lut_2way(ri);
		ri_set_has_lut(ri, false);
		rtt_lut_unref(rr, lut);
		if (++n_found == n_useless) {
			break;
		}
	}
}


/*
 * Forward declaration for lut_fill()
 */
static int lut_want(rttree_t *, rtt_inode_t *);


/*
 * lut_fill()
 *
 * Given a pointer to an LUT and the inode at the root
 * of the spot this LUT will be attached to, fill in the LUT.
 *
 * This function has a hard job.  Beyond determining the contents
 * of the LUT itself the function is also responsible for setting
 * covered bits in the nodes the new LUT is covering, for noticing
 * "useless" LUTs in the nodes it is covering, for keeping track of
 * where covered bits need to be turned off (the uncover_bit argument
 * tells it the extent that existing LUTs cover, but the "useless"
 * LUTs may extend this), and for skipping the `deleted' node when
 * it is encountered.  When it fills in the LUT with a pointer
 * to an inode (uncovered by the current LUT) testing a bit in the
 * same byte it calls lut_want() to see if an LUT should be
 * added there too, perhaps resulting in this function being called
 * recursively to fill in the lower LUT.  As LUTs are completed
 * and filled in in a bottom-up fashion, by the time we finish
 * the topmost LUT everything lower down should have been filled
 * in optimally.
 */
static int
lut_fill (rttree_t *rr, rtt_inode_t *ri, rtt_lut_t lut, rtt_ibit_t uncover_bit)
{
	rtt_ptr_t defs[RN_NBBY];
	rtt_ibit_t covers[RN_NBBY / 2];
	rtt_ibit_t bit, max_bit;
	rtt_ptr_t chp, r_ptr, def_ptr;
	volatile rtt_ptr_t *lp;
	rtt_inode_t *c_ri;
	rtnode_t *rn;
	bool doing_right, ri_max_bit, deleted;
	unsigned int i, ed, sp_def, sp_cov;
	int n_useless;

	/*
	 * The rules for this are as follows:
	 *
	 * 1. We go down the tree in the leftward direction
	 *    looking either for a child node or an inode with
	 *    a bit greater than max_bit.  Starting at the
	 *    left will fill in the LUT in order.
	 *
	 * 2. If we arrive at an inode on the way down which
	 *    has an attached route we fill the LUT
	 *    up to the inode's left edge with the old default,
	 *    then stack that one and use the attached route as
	 *    the new default.
	 *
	 * 3. We then move up the tree looking for an inode where
	 *    we went left, take the right pointer and go back to
	 *    1.
	 *
	 * 4. If we are about to go upward from a node with an
	 *    attached route we fill to the right edge of that
	 *    node with the current default, then unstack a
	 *    replacement.
	 *
	 * The pointers that are copied into a LUT are
	 * pointers either to leaf external nodes, or
	 * to internal nodes which test a bit greater
	 * than max_bit, the bit number of the deepest
	 * inode covered by the LUT.  These pointers will
	 * be child pointers of inodes whose bit number
	 * is less than or equal to max_bit.
	 *
	 * The code is structured as follows.  The top
	 * of the loop assumes that chp holds the pointer
	 * which is the right child of ri.  If this points
	 * to something which belongs in the LUT, doing_right
	 * is set true (and c_ri will be set to rp_is_int(chp)
	 * if chp is an inode) and it drops to the code below
	 * to install this pointer in the LUT.  Otherwise it
	 * searches to the left of c_ri, and each node below,
	 * to find a pointer on the left which belongs in the LUT.
	 * When it finds one, or when it finds an inode with
	 * ri->rtti_bit == max_bit (an optimization; both the
	 * left and right pointers of such inodes can be copied
	 * into the LUT directly, with less work than the more
	 * general case, and at least half of all pointers in
	 * the LUT should come from such nodes) it proceeds to
	 * the code below to install this pointer in the LUT
	 * with doing_right set to false.  chp is the pointer
	 * to install, while ri is set to point at chp's parent.
	 * If ri's right side pointer remains to be installed
	 * (i.e. doing_right is false), the code copies that
	 * pointer into chp and continues back to the top of
	 * the loop.
	 *
	 * If the code installs ri's right hand pointer
	 * (i.e. doing_right is true), however, it moves on to
	 * the code at the bottom.  Since both ri's children are
	 * in the LUT, that code moves back up the tree
	 * (through ri->rtti_parent) looking for an inode where
	 * the right side has not yet been processed.  It sets
	 * ri to that, and chp to the right side child, and
	 * goes back to the top to process chp.  When the code
	 * at the bottom makes it all the way back up to the
	 * root inode (the one where the LUT will be attached)
	 * without finding an unprocessed right side, the walk
	 * completes.
	 *
	 * The "default" pointer is used to fill in spots in the
	 * LUT which are not occupied by the pointers found
	 * above.  The default pointer starts off as RP_NULL.
	 * When the walk finds an attached node on an inode
	 * covered by the tree on the way down (i.e. the code at
	 * the top of the loop) the original default pointer is
	 * pushed on a stack and is replaced by a pointer to the
	 * attached node as the default pointer for the subtree
	 * below.  When the same node is encountered on the way
	 * back up (i.e. the code at the bottom of the loop), the
	 * previous default pointer is popped.
	 *
	 * The default pointer is written into the structure
	 * lazily.  Only when we find a non-default pointer to
	 * write into the structure, or when the default pointer
	 * is about to change, or at the end of the search, do
	 * we fill the LUT with default pointers up to the spot
	 * we need to.  Note that this works because our left-first,
	 * right-last walk through the tree guarantees that the
	 * LUT will be filled in in order, i.e. the index into
	 * the LUT we'll need to write for each node found is
	 * guaranteed to monotonically increase.  When we find
	 * something to write into the LUT, and compute the index
	 * where it belongs, we know that if we haven't written
	 * up to that point already we should fill it in with
	 * the default pointer.
	 *
	 * The underlying data structure is a little bit
	 * challenging to grasp, but is really good at what
	 * it does.
	 *
	 * XXX This should probably be rewritten; fixing the
	 * bits I forgot about when it was first composed has
	 * done it some damage at the top of the loop.  This
	 * seems to work, though, so I'll leave it.
	 */

	/*
	 * Do some preliminary computation of constants
	 */
	lp = lut_ptr(lut);
	bit = ri->rtti_bit;
	max_bit = bit + lut_isize(lut);
	i = sp_cov = sp_def = 0;
	ri_max_bit = false;
	n_useless = 0;

	/*
	 * The default pointer is set to RP_NULL.  We used to
	 * search back up the tree looking for a node attached
	 * to an inode testing a bit in the same byte, but we
	 * no longer make this optimization.
	 */
	def_ptr = RP_NULL;

	/*
	 * Initialize chp to the root inode to enter the loop.  Start
	 * with deleted set true since we'll special-case that code
	 * to keep us from stacking an attached node from the root.
	 */
	c_ri = ri;
	ri_was_seen(ri);
	chp = rp_from_int(ri);
	do {
		if (rp_is_ext(chp) ||
		    (c_ri = rp_to_int(chp))->rtti_bit > max_bit) {
			doing_right = true;
		} else {
			doing_right = false;

			/*
			 * At this point c_ri is an inode which is
			 * covered by the LUT (or may be the root
			 * for which the LUT is being filled).  We
			 * head left looking for something to insert
			 * in the LUT.
			 */
			for (;;) {
				rtt_ibit_t c_bit;

				if (c_ri == ri) {
					/*
					 * We just entered the loop, c_ri
					 * is the root of the tree.  We
					 * ignore the attached node of
					 * the root and any LUT.  Just
					 * go left.  If we get an external
					 * or an uncovered internal
					 * break out to process it, otherwise
					 * it becomes c_ri.
					 */
					chp = ri->rtti_left;
					if (rp_is_ext(chp)) {
						break;
					}
					c_ri = rp_to_int(chp);
					if (c_ri->rtti_bit > max_bit) {
						break;
					}
				}
				ri_set_covered(c_ri, true);
				ri_was_seen(c_ri);
				if (ri_is_deleted(c_ri)) {
					/*
					 * Deleted inode covered
					 * by the LUT.  If the deleted
					 * guy is on the left of his
					 * parent (i.e. ri) we'd like
					 * to leave ri alone and just
					 * process the child.  If it's
					 * on the right, however, we
					 * better break out of here
					 * with doing_right = true or
					 * we'll loop forver.
					 */
					doing_right =
					  (ri->rtti_right == rp_from_int(c_ri));
					chp = c_ri->rtti_attached;
					if (chp != RP_NULL) {
						break;
					}
					chp = c_ri->rtti_left;
					if (chp == RP_NULL) {
						chp = c_ri->rtti_right;
					}
					if (rp_is_ext(chp)) {
						break;
					}
					c_ri = rp_to_int(chp);
					if (c_ri->rtti_bit > max_bit) {
						break;
					}
					doing_right = false;
					continue;
				}

				/*
				 * Here we're at the normal case: covered,
				 * non-root, undeleted inode.  Pay
				 * attention to both an attached node
				 * and an LUT if it has one.
				 */
				ri = c_ri;
				c_bit = ri->rtti_bit;
				if (c_bit == max_bit) {
					ri_max_bit = true;
				}
				r_ptr = ri->rtti_attached;
				if (ri_max_bit || r_ptr != RP_NULL) {
					/*
					 * Advance to the edge of the
					 * inode filling in with the
					 * default.
					 */
					ed = lut_seg_index(bit, max_bit,
							   ri->rtti_byte);
					while (i < ed) {
						lp[i++] = def_ptr;
					}
				}
				if (!ri_max_bit && r_ptr != RP_NULL) {
					/*
					 * On nodes testing bits less
					 * than max_bit with an attached
					 * node we stack the old default
					 * and use the attached node as
					 * a new one.  This will get popped
					 * on the way back up.  There's no
					 * need to do this if the bit number
					 * is ri_max_bit since the attached
					 * node won't appear in more than
					 * one spot in the LUT.
					 */
					defs[sp_def++] = def_ptr;
					def_ptr = r_ptr;
				}
				if (ri_has_lut(ri)) {
					rtt_ibit_t n_uncover_bit;

					/*
					 * Stack the uncover_bit.
					 * If this guy's extends
					 * further, use it instead.
					 */
					covers[sp_cov++] = uncover_bit;
					n_uncover_bit = c_bit +
						    lut_isize(ri->rtti_lut);
					if (n_uncover_bit > uncover_bit) {
						uncover_bit = n_uncover_bit;
					}
					n_useless++;
				}
				chp = ri->rtti_left;
				if (rp_is_ext(chp)) {
					break;
				}
				c_ri = rp_to_int(chp);
				if (c_ri->rtti_bit > max_bit) {
					break;
				}
			}
		}

		/*
		 * At this point chp is either a child external
		 * or an internal somewhere beyond our max_bit.
		 */
		if (rp_is_ext(chp)) {
			/*
			 * A child external.  If it is pointed at by
			 * a node testing max_bit just write it.  Otherwise,
			 * if it isn't RP_NULL fill it in.
			 */
			if (ri_max_bit) {
				if (chp == RP_NULL) {
					/*
					 * This is what lets us not
					 * stack the attached default
					 * above.
					 */
					lp[i++] = ri->rtti_attached;
				} else {
					lp[i++] = chp;
				}
			} else if (chp != RP_NULL) {
				rtt_ibit_t c_bit;
				uint8_t c_byte;

				rn = rp_to_ext(chp);
				c_bit = rn->rttn_len;
				c_byte = rn_key_byte(rn_key(rr, rn), bit);
				if (rn_same_byte(bit, c_bit)) {
					c_byte &= 0xff << RN_BIT(-c_bit);
				}
				ed = lut_seg_index(bit, max_bit, c_byte);
				while (i < ed) {
					lp[i++] = def_ptr;
				}
				ed += lut_seg_length(bit, max_bit, c_bit);
				while (i < ed) {
					lp[i++] = chp;
				}
			}
		} else {
			rtt_ibit_t c_bit = c_ri->rtti_bit;

			/*
			 * Here we've got a child inode, pointed
			 * at by c_ri, which is out beyond the
			 * extent of the LUT.  If we need to,
			 * bring the edge of the LUT up to where
			 * it needs to go.  We don't need to do this
			 * if ri_max_bit is true, however, since we
			 * instead did it above.
			 */
			if (!ri_max_bit) {
				ed = lut_seg_index(bit, max_bit,
						   c_ri->rtti_byte);
				while (i < ed) {
					lp[i++] = def_ptr;
				}
			}

			/*
			 * This guy (or his child, if he's deleted) will be
			 * pointed to by our optimization structure.
			 * Note that this guy was processed.
			 */
			ri_was_seen(c_ri);

			/*
			 * If he's inside the uncover_bit, mark
			 * him and his children uncovered now.
			 */
			if (c_bit <= uncover_bit) {
				lut_uncover(c_ri, c_bit, uncover_bit);
			}

			/*
			 * Write the inode pointer into the LUT.  If it
			 * is deleted, however, write its child into the
			 * LUT instead.  Note that if it is deleted we'll
			 * want to consider running lut_want() on the
			 * child instead so we'll arrange for that.
			 */
			if (ri_is_deleted(c_ri)) {
				r_ptr = c_ri->rtti_attached;
				if (r_ptr == RP_NULL) {
					r_ptr = c_ri->rtti_left;
					if (r_ptr == RP_NULL) {
						r_ptr = c_ri->rtti_right;
					}
				}
				lp[i++] = r_ptr;
				if (rp_is_ext(r_ptr)) {
					/*
					 * Don't run lut_want().
					 */
					c_bit = RN_MAXBIT(bit);
				} else {
					/*
					 * Maybe run lut_want().
					 */
					c_ri = rp_to_int(r_ptr);
					c_bit = c_ri->rtti_bit;
				}
			} else {
				lp[i++] = rp_from_int(c_ri);
			}

			/*
			 * If he's inside our byte, and not in
			 * the last rank, call lut_want() to maybe
			 * add an LUT to this guy (since our LUT will
			 * be landing here).
			 */
			if (c_bit < RN_MAXBIT(bit)) {
				n_useless += lut_want(rr, c_ri);
			}
		}

		/*
		 * If we did the left side, set chp to the
		 * right pointer and continue.
		 */
		if (!doing_right) {
			chp = ri->rtti_right;
			continue;
		}

		/*
		 * If we've arrived here we've filled in the LUT
		 * for both sides of the inode ri.  Move back up
		 * looking for a place we went left and go right
		 * instead.  If we see an attached node on the way
		 * up, however, we'll need to fill in with the
		 * current default pointer to his right edge and
		 * then pop the default stack before continuing.
		 */
		doing_right = false;	/* exit flag */
		deleted = false;	/* used to skip deleted nodes */
		do {
			rtt_ibit_t ri_bit = ri->rtti_bit;

			if (ri_max_bit || deleted) {
				/*
				 * Ignore any attached here (like we did
				 * on the way down).  Set ri_max_bit to
				 * false.
				 */
				ri_max_bit = false;
			} else if (ri->rtti_attached != RP_NULL ||
				   ri_bit <= bit) {
				ed = lut_seg_end_index(bit, max_bit, ri_bit,
						       ri->rtti_byte);
				while (i < ed) {
					lp[i++] = def_ptr;
				}
				if (ri_bit <= bit) {
					doing_right = true;	/* all done */
					break;
				}
				def_ptr = defs[--sp_def];
			}
			if (ri_has_lut(ri) && !deleted) {
				uncover_bit = covers[--sp_cov];
			}
			c_ri = ri;
			ri = ri->rtti_parent;
			deleted = ri_is_deleted(ri);
		} while (deleted || ri->rtti_right == rp_from_int(c_ri));
		chp = ri->rtti_right;	/* for next go-around */
	} while (!doing_right);

	/* All done! */
	return (n_useless);
}


/*
 * lut_want()
 *
 * This function is called to consider adding or changing
 * the size of a LUT on the offered inode, or perhaps its
 * children.  It is generally called when there is a reason
 * to think that the inode offered might not have an appropriate
 * size LUT already, e.g. because an LUT above it in the tree
 * changed size and now lands here or because the inode was
 * just added to the tree.  Whatever the reason, the function
 * evaluates the LUT size the node should have and, if it is
 * different than the size of the LUT it already has, allocates
 * memory for the new LUT and then calls lut_fill() to fill it in.
 *
 * The function walks down the tree evaluating the size of
 * an LUT each node should have.  It prunes its search whenever
 * it finds a node with a LUT (after checking to see if the
 * LUT's size should change; if it should the function calls
 * lut_fill() to do that).
 */
static int
lut_want (rttree_t *rr, rtt_inode_t *p_ri)
{
	rtt_lut_t lut;
	rtt_inode_t *ri;
	rtt_ibit_t bit, p_bit, max_bit;
	bool prune;
	int n_useless;

	/*
	 * We walk around the tree, rooted at p_ri, looking
	 * for spots where a LUT needs to change size.  If we
	 * find a spot where we need to change the LUT we call
	 * lut_fill() to initialize it and let that function deal
	 * with everything below.
	 */
	n_useless = 0;
	p_bit = p_ri->rtti_bit;
	max_bit = RN_MAXBIT(p_bit) - 1;
	if (p_bit > max_bit) {
		return (0);
	}
	ri = p_ri;
	do {
		prune = false;
		bit = ri->rtti_bit;
		rtt_size_t n_size;

		/*
		 * Note that this was looked at.  If it is a deleted
		 * node just continue past it.
		 */
		ri_was_seen(ri);
		if (ri_is_deleted(ri)) {
			continue;
		}

		/*
		 * Get the existing LUT.  Determine the desired
		 * size for an LUT on this node.
		 */
		lut = ri->rtti_lut;
		n_size = lut_newsize(rr, ri, bit, lut_size(lut));
		if (n_size != lut_size(lut)) {
			rtt_ibit_t cover_bit;
			rtt_lut_t n_lut;

			/*
			 * The LUT needs to change size.
			 */
			cover_bit = bit + lut_isize(lut);
			if (n_size == LUT_2) {
				/*
				 * Removing the LUT altogether.  Uncover
				 * everything below here.
				 */
				lut_uncover(ri, bit, cover_bit);
				n_lut = lut_2way(ri);
			} else {
				n_lut = rtt_lut_alloc(rr, n_size);
				if (n_lut == LUT_NULL) {
					/*
					 * Give up.  The structure
					 * is correct, just not optimal.
					 */
					return (n_useless);
				}
				n_useless += lut_fill(rr, ri, n_lut,
						      cover_bit);
			}

			/*
			 * Just finished an adjustment.  Make
			 * sure what was done already is visible,
			 * then write the new LUT into the inode.
			 */
			RTT_WMB();
			ri->rtti_lut = n_lut;	/* structure change */
			ri_set_has_lut(ri, (n_size != LUT_2));
			if (lut_size(lut) != LUT_2) {
				rtt_lut_unref(rr, lut);
			}
			lut = n_lut;
		}

		/*
		 * If this node now has a LUT, prune here.  Otherwise
		 * investigate further down.
		 */
		prune = (lut_size(lut) != LUT_2);
	} while ((ri = ri_next_max(ri, p_bit, max_bit, prune)));

	/*
	 * Return anything useless that we found.
	 */
	return (n_useless);
}


/*
 * lut_remove()
 *
 * Remove a LUT from a node.  If the node's children are
 * uncovered try to add LUTs to them first.
 */
static int
lut_remove (rttree_t *rr, rtt_inode_t *ri)
{
	rtt_ibit_t bit, max_bit;
	rtt_lut_t lut;
	unsigned int i;
	int n_useless;

	bit = ri->rtti_bit;
	lut = ri->rtti_lut;
	if (lut_size(lut) == LUT_2) {
		return (0);
	}
	max_bit = bit + lut_isize(lut);
	n_useless = 0;
	for (i = RTT_LEFT; i <= RTT_RIGHT; i++) {
		rtt_ptr_t ptr;
		rtt_inode_t *c_ri;
		rtt_ibit_t c_bit;

		ptr = ri->rtti_child[i];
		if (!rp_is_int(ptr)) {
			continue;
		}
		c_ri = rp_to_int(ptr);
		c_bit = c_ri->rtti_bit;
		if (c_bit > max_bit) {
			continue;
		}
		lut_uncover(c_ri, c_bit, max_bit);
		n_useless += lut_want(rr, c_ri);
	}

	/*
	 * Make sure everything done before is visible before
	 * changing the LUT.
	 */
	RTT_WMB();
	ri->rtti_lut = lut_2way(ri);
	/*
	 * Don't free the LUT here, let the caller do it.
	 */
	/* rtt_lut_unref(rr, lut); */
	return (n_useless);
}



/*
 * lut_optimize()
 *
 * Given an inode in the tree which has recently changed, or is
 * somehow associated with a change, determine if the optimization
 * structures around it can be improved.  Note that the code expects
 * that the existing optimization structures already correctly
 * reflect the change (so it is acceptable to do nothing) except
 * for the fact that an about-to-be-deleted inode is still in the
 * tree (it is a one-way branch at this point) while a pointer to
 * a newly-added inode may not yet have been inserted in a current
 * LUT.  It is done this way to allow us to build and add new LUTs
 * before the inode change is made complete to avoid concurrent
 * lookups from seeing a suboptimal tree, the worst case of which
 * would be deleting an inode with a large LUT.  By leaving the
 * node in, concurrent lookups can continue to use the LUT until
 * we've placed new ones on surrounding nodes.
 *
 * The code expects the the `target' flag to be set if the inode
 * is newly added, and the `target' and `delete' flags to be
 * set if the inode is going away.  If the `target' bit is unset
 * it assumes the inode was already in the tree but we're being
 * called to see if some sub-optimality caused by a prior LUT
 * allocation failure can be corrected.  On return the `target'
 * flag will be clear and the inode will be referenced (or no
 * longer referenced, in the case of a delete) by the LUT optimization
 * structure.  Updating the basic structure to complete the removal
 * of a deleted inode remains to be done.
 */
static void
lut_optimize (rttree_t *rr, rtt_inode_t *o_ri)
{
	rtt_inode_t *ri, *ch_ri, *want_ri;
	rtt_size_t ch_size;
	rtt_ibit_t o_bit, bit, min_bit;
	rtt_lut_t lut;
	rtt_ptr_t r_ptr;
	bool deleted, change;
	int n_useless;

	/*
	 * Figure out why we're here from his flags.
	 */
	o_bit = o_ri->rtti_bit;
	want_ri = NULL;
	if (!ri_is_target(o_ri)) {
		/*
		 * Optimize only.  Search up from the inode
		 * itself.
		 */
		ri = o_ri;
		change = deleted = false;
		r_ptr = RP_NULL;
	} else if ((deleted = ri_is_deleted(o_ri))) {
		/*
		 * A delete.  We need a replacement pointer,
		 * should we need to update an existing LUT, and
		 * we need to search up from the parent to fix
		 * counts and look for a node in need of reoptimization.
		 * Replacement is the node's child.  If the deleted node
		 * has a LUT itself we need to uncover its children
		 * now (the LUT is going away).  We may want
		 * to attempt to add an LUT to that child as
		 * well.
		 */
		change = true;
		ri = o_ri->rtti_parent;
		r_ptr = o_ri->rtti_attached;
		if (r_ptr == RP_NULL) {
			r_ptr = o_ri->rtti_left;
		}
		if (r_ptr == RP_NULL) {
			r_ptr = o_ri->rtti_right;
		}
		if (rp_is_int(r_ptr)) {
			rtt_inode_t *c_ri;

			c_ri = rp_to_int(r_ptr);
			/*
			 * If the bit number of the deleted inode
			 * is in a different byte than his parent
			 * we need to reset the child's byte.  `ri'
			 * points at his parent.
			 */
			bit = ri->rtti_bit;
			if (!rn_same_byte(bit, o_bit)) {
				ri_set_byte(rr, c_ri, bit);
			}
			bit = c_ri->rtti_bit;
			if (bit < RN_MAXBIT(o_bit)) {
				want_ri = c_ri;
			}
			lut = o_ri->rtti_lut;
			if (lut_size(lut) != LUT_2) {
				rtt_ibit_t cover_bit;

				cover_bit = o_bit + lut_isize(lut);
				if (bit <= cover_bit) {
					lut_uncover(c_ri, bit, cover_bit);
				}
			}
		}
	} else {
		rtt_inode_t *c_ri;

		/*
		 * An add.  Replacement pointer is itself (if it isn't
		 * referenced from an LUT yet).  We may want to add an LUT
		 * to it.
		 *
		 * We have a couple of other bookkeeping projects.  We
		 * need to set the inode byte based on his parent's bit.
		 * If his bit is in the same byte as his parent we may
		 * also need to set the covered flag, if it isn't we
		 * may need to reset a child inode's pointer.  Oh, and
		 * if we have a child inode we may need to add in
		 * that inode's counts.
		 */
		change = true;

		r_ptr = o_ri->rtti_left;
		if (!rp_is_int(r_ptr)) {
			r_ptr = o_ri->rtti_right;
		}
		if (rp_is_int(r_ptr)) {
			c_ri = rp_to_int(r_ptr);
		} else {
			c_ri = NULL;
		}

		ri = o_ri->rtti_parent;
		bit = ri->rtti_bit;
		ri_set_byte(rr, o_ri, bit);
		if (rn_same_byte(bit, o_bit)) {
			if (ri_is_lut_covered(ri)) {
				rtt_inode_t *p_ri = ri;
				rtt_ibit_t max_bit;

				while (!ri_has_lut(p_ri)) {
					p_ri = p_ri->rtti_parent;
				}
				max_bit = p_ri->rtti_bit;
				max_bit += lut_isize(p_ri->rtti_lut);
				if (o_bit <= max_bit) {
					ri_set_covered(o_ri, true);
				}
			}
		} else if (c_ri) {
			ri_set_byte(rr, c_ri, o_bit);
		}

		if (c_ri) {
			cnt_add(o_ri, c_ri);	/* does same byte check */
		}

		r_ptr = rp_from_int(o_ri);
		if (o_bit < RN_MAXBIT(o_bit)) {
			want_ri = o_ri;
		}
	}


	/*
	 * Work our way up the tree, starting from `ri' and
	 * stopping if we pass out of the bit.  If it is a change
	 * correct the counts to account for it.  If the node is
	 * uncovered determine whether a different sized LUT
	 * would be appropriate.  Record the latter.
	 */
	ch_ri = NULL;
	ch_size = LUT_1;
	min_bit = RN_MINBIT(o_bit);
	for ( ; ri && (bit = ri->rtti_bit) >= min_bit; ri = ri->rtti_parent) {
		rtt_size_t ri_size;

		if (change) {
			cnt_inc_dec(ri, o_bit, deleted);
		}
		if (ri_is_covered(ri)) {
			continue;
		}
		lut = ri->rtti_lut;
		ri_size = lut_newsize(rr, ri, bit, lut_size(lut));
		if (ri_size != lut_size(lut)) {
			ch_ri = ri;
			ch_size = ri_size;
		}
	}


	/*
	 * See if that found anything.  If so, attempt to
	 * adjust the size of the inode's LUT.
	 */
	n_useless = 0;
	if (ch_ri) {
		rtt_lut_t o_lut;
		rtt_ibit_t cover_bit;

		bit = ch_ri->rtti_bit;
		o_lut = ch_ri->rtti_lut;
		cover_bit = bit + lut_isize(o_lut);
		if (ch_size == LUT_2) {
			/* Remove his LUT */
			n_useless += lut_remove(rr, ch_ri);
			ri_set_has_lut(ch_ri, false);
		} else  if ((lut = rtt_lut_alloc(rr, ch_size)) != LUT_NULL) {
			n_useless += lut_fill(rr, ch_ri, lut, cover_bit);

			/* Ensure global visibility, then change it */
			RTT_WMB();
			ch_ri->rtti_lut = lut;
			ri_set_has_lut(ch_ri, true);
		} else {
			/* Note that alloc() failed */
			o_lut = lut_make(NULL, LUT_2);
		}
		if (lut_size(o_lut) != LUT_2) {
			rtt_lut_unref(rr, o_lut);
		}
	}

	/*
	 * If the above work reached our changed inode it will
	 * have turned off the target bit (or if this was just an
	 * optimization it will never have had one) and the work
	 * will be complete with only final cleanup being required.
	 */
	if (ri_is_target(o_ri)) {
		ri_set_target(o_ri, false);	/* turn it off */

		/*
		 * No such luck, we'll need to write the inode into
		 * any LUT pointing at it ourself.  Before doing this,
		 * however, see if we can usefully add an LUT directly
		 * where it is needed before we add this to the search
		 * path.
		 */
		if (want_ri && !ri_is_covered(want_ri)) {
			if (n_useless == 0) {
				ch_ri = want_ri;
			}
			n_useless += lut_want(rr, want_ri);
		}

		/*
		 * Now we need to determine if an LUT points, or should
		 * point, at the inode and, if so, update it appropriately
		 * with the replacement pointer we determined above.
		 */
		ri = o_ri->rtti_parent;
		if (ri && ri_is_lut_covered(ri) && !ri_is_covered(o_ri)) {
			rtt_ibit_t max_bit;
			unsigned int ed;

			while (!ri_has_lut(ri)) {
				ri = ri->rtti_parent;
			}
			bit = ri->rtti_bit;
			lut = ri->rtti_lut;
			max_bit = bit + lut_isize(lut);
			ed = lut_seg_index(bit, max_bit, o_ri->rtti_byte);

			/*
			 * Ensure global visibility of prior, then
			 * change structure
			 */
			RTT_WMB();
			lut_ptr(lut)[ed] = r_ptr;
		}
	}

	/*
	 * All done but the cleanup.  If the changes left useless
	 * entries behind, clean them out.
	 */
	RTT_WMB();
	if (n_useless > 0) {
		lut_cleanup(rr, ch_ri, n_useless);
	}

	/*
	 * If this was a delete and the node leaving us has
	 * an LUT, free it.
	 */
	if (deleted) {
		lut = o_ri->rtti_lut;
		if (lut_size(lut) != LUT_2) {
			o_ri->rtti_lut = lut_2way(o_ri);
			rtt_lut_unref(rr, lut);
			ri_set_has_lut(o_ri, false);
		}
	}

	/* That's all */
}


/*
 * Existing structure adjustment here.  Fixes up attached parent
 * pointers and rewrites LUTs to account for an external node
 * change.
 */

/*
 * ri_set_aparent()
 *
 * Set the rtti_aparent pointers in inodes below the argument inode
 * to the specified value.  Prune at inodes with attached routes
 * further down the tree.
 *
 * This could execute for a long time if the structure has many
 * externals and root_ri->rtti_bit is small.
 */
static inline void ri_set_aparent(rtt_inode_t *, rtt_inode_t *) ALWAYS_INLINE;

static inline void
ri_set_aparent (rtt_inode_t *root_ri, rtt_inode_t *ap_ri)
{
	rtt_ibit_t bit;
	rtt_inode_t *ri;
	bool prune;

	bit = root_ri->rtti_bit;
	for (prune = false, ri = root_ri;
	   (ri = ri_next(ri, bit, prune)) != NULL;
	    prune = (ri->rtti_attached != RP_NULL)) {
		ri->rtti_aparent = ap_ri;
	}
}



/*
 * rn_replace()
 *
 * This function takes two external nodes as arguments and
 * replaces any existing references to (or depending on) the
 * old external with references to the new external in the
 * optimization structures.  One or the other external can
 * be NULL (that would be an add or a delete rather than
 * a change); if they are both non-NULL then both will have
 * the same key and key length.
 *
 * The function replaces references to the old external with
 * the new one in the LUT at or above its parent inode, if there is
 * one.  If it is attached, and is being added or deleted (i.e.
 * one or the other of the two external node arguments is NULL),
 * is will also update the attached parent pointers below it in
 * the tree.
 *
 * This function can't trust the flags set on its parent inode
 * (it might have been recently added).
 */
static void
rn_replace (rttree_t *rr, rtnode_t *new_rn, rtnode_t *old_rn)
{
	rtt_inode_t *p_ri, *ri, *a_ri;
	rtnode_t *rn;
	rtt_ibit_t bit, p_bit, l_bit, l_maxbit;
	rtt_ptr_t n_ptr, o_ptr;
	uint8_t byte;
	rtt_lut_t lut;
	unsigned int l_len;
	rtt_ptr_t *lp;

	/*
	 * Check for sanity.  This also ensures that at least
	 * one is not NULL.
	 */
	if (new_rn == old_rn) {
		return;		/* nothing to do?!? */
	}

	/*
	 * Get a pointer to whichever one is non-NULL.  Prefer
	 * the new guy.  Find the parent inode.
	 */
	if ((rn = new_rn) == NULL) {
		rn = old_rn;
	}
	bit = rn->rttn_len;
	p_ri = rn->rttn_parent;
	p_bit = p_ri->rtti_bit;

	/*
	 * Determine if the parent has a LUT or is covered
	 * by one.  We can't trust the covered flag in the parent
	 * itself, however, so we'll search back up the tree looking
	 * for the lut.  If we find one in the same byte make sure
	 * it covers the parent.
	 */
	l_bit = 0;
	for (ri = p_ri; ri; ri = ri->rtti_parent) {
		l_bit = ri->rtti_bit;
		if (!rn_same_byte(l_bit, p_bit) ||
		    (ri != p_ri && !ri_is_lut_covered(ri))) {
			/*
			 * There is no LUT.  We can trust the
			 * covered flag on everything but the parent.
			 */
			ri = NULL;
			break;
		}
		if (ri_has_lut(ri)) {
			break;
		}
	}

	lut = 0;
	l_maxbit = 0;
	if (ri) {
		lut = ri->rtti_lut;
		l_maxbit = l_bit + lut_isize(lut);
		if (l_maxbit < p_bit) {
			ri = NULL;
		}
	}

	/*
	 * Look for a quick exit.  If the node is not attached,
	 * his parent isn't covered and has no LUT itself the
	 * external will only be referenced from the basic parent
	 * inode pointers and someone else will take care of that.
	 * If it is an attached add or delete we'll need to update
	 * the attached parent pointers below it in the tree (a
	 * potentially big job, but simple), but needn't do anything
	 * else if its parent isn't covered.
	 *
	 * If we're not so lucky fetch the byte from the external
	 * in question, we'll need it soon.
	 */
	if (p_bit == bit) {
		/*
		 * Attached.  If it is an add or delete, and if the
		 * parent inode is not the root inode, then we'll need
		 * to adjust the parent pointers below.
		 */
		a_ri = p_ri->rtti_aparent;
		if ((!new_rn || !old_rn) && a_ri != NULL) {
			ri_set_aparent(p_ri, ((new_rn != NULL) ? p_ri : a_ri));
		}

		/*
		 * If the parent inode isn't covered by an LUT above
		 * him we won't be mentioned in one, not even the parent's
		 * LUT.
		 */
		if (ri == NULL || ri == p_ri) {
			return;
		}
	} else {
		/*
		 * Child external.  The guy might be referenced in an
		 * LUT if his parent inode has one or is covered.
		 */
		if (ri == NULL) {
			return;
		}

		if (p_ri->rtti_attached != RP_NULL) {
			a_ri = p_ri;
		} else {
			a_ri = p_ri->rtti_aparent;
		}
	}


	/*
	 * We're going to have to inspect the lut.  Fetch the
	 * appropriate byte from the key to index the LUT.
	 */
	byte = rn_key_byte(rn_key(rr, rn), p_bit);
	if (rn_same_byte(p_bit, bit)) {
		/*
		 * Bit tests same byte as parent.  Zero out the
		 * low order end of the byte to the length of
		 * our bit.
		 */
		byte &= 0xff << RN_BIT(-bit);
	}

	/*
	 * We need a replacement for the node, or to identify
	 * the thing being replaced.  a_ri points at something
	 * which should have an attached node, that node will
	 * be our guy if it is also covered by the LUT.
	 */
	if (a_ri && a_ri->rtti_bit <= l_bit) {
		a_ri = NULL;
	}

	/*
	 * Figure out the pointer we'll be replacing, and the pointer
	 * we'll be replacing it with, if we find an LUT.
	 */
	if (new_rn) {
		n_ptr = rp_from_ext(new_rn);
	} else if (a_ri) {
		n_ptr = a_ri->rtti_attached;
	} else {
		n_ptr = RP_NULL;
	}

	if (old_rn) {
		o_ptr = rp_from_ext(old_rn);
	} else if (a_ri) {
		o_ptr = a_ri->rtti_attached;
	} else {
		o_ptr = RP_NULL;
	}

	/*
	 * Figure out the segment we're in and replace the old pointer
	 * with the new one.
	 */
	lp = lut_ptr(lut) + lut_seg_index(l_bit, l_maxbit, byte);
	l_len = lut_seg_length(l_bit, l_maxbit, bit);
	while (l_len > 0) {
		if (*lp == o_ptr) {
			*lp = n_ptr;
		}
		lp++;
		l_len--;
	}

	/* All done! */
}


/*
 * rttree_update_()
 *
 * This is the sole external entry point in this file, and is
 * intended to be called when the rttree has been modified.  It
 * is intended to be used only by the internal callers.  It is
 * called with two external node arguments; if both are non-NULL
 * then both must have the same key and key length.  `new_rn' is an
 * external which is newly pointed to by one of the pointers
 * (2 child, 1 attached) in the inode that is its parent, while
 * `old_rn' is an external which was recently pointed to by one
 * of those pointers but now is not.  If one of these external
 * pointers is NULL then the operation is a simple add or delete
 * of the destination, otherwise it is a change.  In addition, if
 * the inode which is the external's parent is changing (either
 * newly added or about to be deleted) it will have the
 * `target' flag set.  If it is being deleted it will additionally
 * have the `deleted' flag set; the node will still be in the
 * tree but references to `old_rn' will have been removed, turning
 * the node into a one-way branch but leaving its LUT, should it
 * have one, in the tree.
 *
 * The function performs three operations.  It modifies existing
 * LUTs to remove references to `old_rn' and add references to `new_rn'
 * in the appropriate spots and it updates attached parent pointers
 * in inodes below the change, should it need to.  This corrects
 * forwarding with the existing LUT arrangement.  It then considers
 * making modifications to the existing LUT arrangement by shrinking
 * or growing LUTs.  In general the latter changes will only be
 * found necessary if an inode is being added or deleted, but
 * the area of the tree effected by a change is always inspected
 * since this may allow us to recover an optimal tree state if
 * there have been previous LUT allocation failures.
 *
 * rn_replace() is called to modify the existing LUTs, followed
 * by a call to lut_optimize() to deal with an inode change (if
 * any) and reevaluate the state of the tree in the neighbourhood.
 * If `old_rn' and `new_rn' point at the same external the call
 * to rn_replace() is skipped and several calls to lut_optimize()
 * are made on the path to the root of the tree; the latter might
 * be useful to force a reoptimization if it is known that LUT
 * allocation failures have occured in the past.
 */
void
rttree_update_ (rttree_t *rr, rtnode_t *new_rn, rtnode_t *old_rn)
{
	rtt_inode_t *ri;

	if (new_rn != old_rn) {
		ri = (new_rn ? new_rn->rttn_parent : old_rn->rttn_parent);
		rn_replace(rr, new_rn, old_rn);
		lut_optimize(rr, ri);
	} else if (new_rn) {
		rtt_ibit_t bit, o_bit;

		ri = new_rn->rttn_parent;
		do {
			lut_optimize(rr, ri);
			o_bit = ri->rtti_bit;
			while ((ri = ri->rtti_parent) != NULL) {
				bit = ri->rtti_bit;
				if (!rn_same_byte(o_bit, bit)) {
					break;
				}
			}
		} while (ri);
	}
}
