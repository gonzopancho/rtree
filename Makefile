#
# Makefile for librttree.a
#

LIB=	librttree.a

SRCS=	rttree.h rttree_add.c rttree_change.c rttree_defs.h \
	rttree_delete.c rttree_destroy.c \
	rttree_find.c rttree_find.h rttree_init.c \
	rttree_getnext.c rttree_getnext.h rttree_getnext_v.c \
	rttree_getprev.c rttree_getprev_v.c \
	rttree_lookup.c rttree_lookup.h rttree_lookup_c.c rttree_lookup_cx.c \
	rttree_lookup_x.c rttree_lookup_v.c rttree_lookup_c_v.c \
	rttree_lookup_cx_v.c rttree_lookup_x_v.c \
	rttree_next.c rttree_next.h rttree_next_c.c \
	rttree_next_cx.c rttree_next_x.c \
	rttree_next_v.c rttree_next_c_v.c \
	rttree_next_cx_v.c rttree_next_x_v.c \
	rttree_plookup.c rttree_plookup_c.c \
	rttree_plookup_cx.c rttree_plookup_x.c \
	rttree_plookup_v.c rttree_plookup_c_v.c \
	rttree_plookup_cx_v.c rttree_plookup_x_v.c \
	rttree_prev.c rttree_prev_c.c \
	rttree_prev_cx.c rttree_prev_x.c \
	rttree_prev_v.c rttree_prev_c_v.c \
	rttree_prev_cx_v.c rttree_prev_x_v.c \
	rttree_prune_next.c rttree_prune_next_c.c \
	rttree_prune_next_cx.c rttree_prune_next_x.c \
	rttree_prune_next_v.c rttree_prune_next_c_v.c \
	rttree_prune_next_cx_v.c rttree_prune_next_x_v.c \
	rttree_prune_prev.c rttree_prune_prev_c.c \
	rttree_prune_prev_cx.c rttree_prune_prev_x.c \
	rttree_prune_prev_v.c rttree_prune_prev_c_v.c \
	rttree_prune_prev_cx_v.c rttree_prune_prev_x_v.c \
	rttree_sanity.c rttree_sanity.h rttree_stats_init.c \
	rttree_subtree.h \
	rttree_sub_first.c rttree_sub_first_c.c \
	rttree_sub_first_cx.c rttree_sub_first_x.c \
	rttree_sub_first_v.c rttree_sub_first_c_v.c \
	rttree_sub_first_cx_v.c rttree_sub_first_x_v.c \
	rttree_sub_last.c rttree_sub_last_c.c \
	rttree_sub_last_cx.c rttree_sub_last_x.c \
	rttree_sub_last_v.c rttree_sub_last_c_v.c \
	rttree_sub_last_cx_v.c rttree_sub_last_x_v.c \
	rttree_sub_next.c rttree_sub_next_c.c \
	rttree_sub_next_cx.c rttree_sub_next_x.c \
	rttree_sub_next_v.c rttree_sub_next_c_v.c \
	rttree_sub_next_cx_v.c rttree_sub_next_x_v.c \
	rttree_sub_prev.c rttree_sub_prev_c.c \
	rttree_sub_prev_cx.c rttree_sub_prev_x.c \
	rttree_sub_prev_v.c rttree_sub_prev_c_v.c \
	rttree_sub_prev_cx_v.c rttree_sub_prev_x_v.c \
	rttree_release.c rttree_tune.c rttree_update.c rttree_update.h

OTHERS=	Makefile rttree.3 rttree_v.3 # rttree.3.pdf rttree_v.3.pdf

OBJS=	rttree_add.o rttree_change.o rttree_delete.o \
	rttree_destroy.o \
	rttree_find.o \
	rttree_getnext.o rttree_getnext_v.o \
	rttree_getprev.o rttree_getprev_v.o \
	rttree_init.o \
	rttree_lookup.o rttree_lookup_c.o rttree_lookup_cx.o \
	rttree_lookup_x.o rttree_lookup_v.o rttree_lookup_c_v.o \
	rttree_lookup_cx_v.o rttree_lookup_x_v.o \
	rttree_next.o rttree_next_c.o rttree_next_cx.o rttree_next_x.o \
	rttree_next_v.o rttree_next_c_v.o rttree_next_cx_v.o rttree_next_x_v.o \
	rttree_plookup.o rttree_plookup_c.o \
	rttree_plookup_cx.o rttree_plookup_x.o \
	rttree_plookup_v.o rttree_plookup_c_v.o \
	rttree_plookup_cx_v.o rttree_plookup_x_v.o \
	rttree_prev.o rttree_prev_c.o rttree_prev_cx.o rttree_prev_x.o \
	rttree_prev_v.o rttree_prev_c_v.o rttree_prev_cx_v.o rttree_prev_x_v.o \
	rttree_prune_next.o rttree_prune_next_c.o \
	rttree_prune_next_cx.o rttree_prune_next_x.o \
	rttree_prune_next_v.o rttree_prune_next_c_v.o \
	rttree_prune_next_cx_v.o rttree_prune_next_x_v.o \
	rttree_prune_prev.o rttree_prune_prev_c.o \
	rttree_prune_prev_cx.o rttree_prune_prev_x.o \
	rttree_prune_prev_v.o rttree_prune_prev_c_v.o \
	rttree_prune_prev_cx_v.o rttree_prune_prev_x_v.o \
	rttree_sanity.o rttree_stats_init.o \
	rttree_sub_first.o rttree_sub_first_c.o \
	rttree_sub_first_cx.o rttree_sub_first_x.o \
	rttree_sub_first_v.o rttree_sub_first_c_v.o \
	rttree_sub_first_cx_v.o rttree_sub_first_x_v.o \
	rttree_sub_last.o rttree_sub_last_c.o \
	rttree_sub_last_cx.o rttree_sub_last_x.o \
	rttree_sub_last_v.o rttree_sub_last_c_v.o \
	rttree_sub_last_cx_v.o rttree_sub_last_x_v.o \
	rttree_sub_next.o rttree_sub_next_c.o \
	rttree_sub_next_cx.o rttree_sub_next_x.o \
	rttree_sub_next_v.o rttree_sub_next_c_v.o \
	rttree_sub_next_cx_v.o rttree_sub_next_x_v.o \
	rttree_sub_prev.o rttree_sub_prev_c.o \
	rttree_sub_prev_cx.o rttree_sub_prev_x.o \
	rttree_sub_prev_v.o rttree_sub_prev_c_v.o \
	rttree_sub_prev_cx_v.o rttree_sub_prev_x_v.o \
	rttree_release.o rttree_tune.o rttree_update.o

TARFILE=	rttree.tgz

# CFLAGS= -I. $(ARCH) -g -O0 -Wall -Wextra -Wno-padded # -Weverything
# CFLAGS= -I. $(ARCH) -g -Wall -Wextra -O3 # -Weverything 
CFLAGS= -I. $(ARCH) -g -Wall -Wextra -Wno-padded -O3 # -Weverything 
# CFLAGS= -I. $(ARCH) -g -Wall -Wextra -O3 -pg # -Weverything


$(LIB):	$(OBJS)
	@echo building static ${LIB} library
	@rm -f $(LIB)
	@ar cq $(LIB) `lorder ${OBJS} | tsort -q`
	ranlib $(LIB)

rttree_lookup.s:	rttree_lookup.c rttree.h rttree_defs.h rttree_lookup.h
	cc $(CFLAGS) -S rttree_lookup.c

clean:
	rm -f $(OBJS) $(LIB) $(TARFILE)

tar:	$(TARFILE)

$(TARFILE):	$(SRCS) $(OTHERS)
	@echo "(re-)creating" $(TARFILE)
	@rm -f $(TARFILE)
	@tar czf $(TARFILE) $(SRCS) $(OTHERS)

RTTREE_DEFS=		rttree.h rttree_defs.h
RTTREE_UPDATE_DEFS=	$(RTTREE_DEFS) rttree_update.h
RTTREE_LOOKUP_DEFS=	$(RTTREE_DEFS) rttree_lookup.h
RTTREE_FIND_DEFS=	$(RTTREE_DEFS) rttree_find.h
RTTREE_NEXT_DEFS=	$(RTTREE_DEFS) rttree_next.h
RTTREE_GETNEXT_DEFS=	$(RTTREE_DEFS) rttree_getnext.h
RTTREE_SUBTREE_DEFS=	$(RTTREE_NEXT_DEFS) rttree_subtree.h

rttree_add.o:		rttree_add.c $(RTTREE_UPDATE_DEFS)
rttree_change.o:	rttree_change.c $(RTTREE_UPDATE_DEFS)
rttree_delete.o:	rttree_delete.c $(RTTREE_UPDATE_DEFS)
rttree_destroy.o:	rttree_destroy.c $(RTTREE_UPDATE_DEFS)

rttree_find.o:		rttree_find.c $(RTTREE_FIND_DEFS)

rttree_getnext.o:	rttree_getnext.c $(RTTREE_GETNEXT_DEFS)
rttree_getnext_v.o:	rttree_getnext_v.c $(RTTREE_GETNEXT_DEFS)

rttree_getprev.o:	rttree_getprev.c $(RTTREE_GETNEXT_DEFS)
rttree_getprev_v.o:	rttree_getprev_v.c $(RTTREE_GETNEXT_DEFS)

rttree_init.o:		rttree_init.c $(RTTREE_UPDATE_DEFS)

rttree_lookup.o:	rttree_lookup.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_c.o:	rttree_lookup_c.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_cx.o:	rttree_lookup_cx.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_x.o:	rttree_lookup_x.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_v.o:	rttree_lookup_v.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_c_v.o:	rttree_lookup_c_v.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_cx_v.o:	rttree_lookup_cx_v.c $(RTTREE_LOOKUP_DEFS)
rttree_lookup_x_v.o:	rttree_lookup_x_v.c $(RTTREE_LOOKUP_DEFS)

rttree_next.o:		rttree_next.c $(RTTREE_NEXT_DEFS)
rttree_next_c.o:	rttree_next_c.c $(RTTREE_NEXT_DEFS)
rttree_next_cx.o:	rttree_next_cx.c $(RTTREE_NEXT_DEFS)
rttree_next_x.o:	rttree_next_x.c $(RTTREE_NEXT_DEFS)

rttree_next_v.o:	rttree_next_v.c $(RTTREE_NEXT_DEFS)
rttree_next_c_v.o:	rttree_next_c_v.c $(RTTREE_NEXT_DEFS)
rttree_next_cx_v.o:	rttree_next_cx_v.c $(RTTREE_NEXT_DEFS)
rttree_next_x_v.o:	rttree_next_x_v.c $(RTTREE_NEXT_DEFS)

rttree_plookup.o:	rttree_plookup.c $(RTTREE_LOOKUP_DEFS)
rttree_plookup_c.o:	rttree_plookup_c.c $(RTTREE_LOOKUP_DEFS)
rttree_plookup_cx.o:	rttree_plookup_cx.c $(RTTREE_LOOKUP_DEFS)
rttree_plookup_x.o:	rttree_plookup_x.c $(RTTREE_LOOKUP_DEFS)

rttree_plookup_v.o:	rttree_plookup_v.c $(RTTREE_LOOKUP_DEFS)
rttree_plookup_c_v.o:	rttree_plookup_c_v.c $(RTTREE_LOOKUP_DEFS)
rttree_plookup_cx_v.o:	rttree_plookup_cx_v.c $(RTTREE_LOOKUP_DEFS)
rttree_plookup_x_v.o:	rttree_plookup_x_v.c $(RTTREE_LOOKUP_DEFS)

rttree_prev.o:		rttree_prev.c $(RTTREE_NEXT_DEFS)
rttree_prev_c.o:	rttree_prev_c.c $(RTTREE_NEXT_DEFS)
rttree_prev_cx.o:	rttree_prev_cx.c $(RTTREE_NEXT_DEFS)
rttree_prev_x.o:	rttree_prev_x.c $(RTTREE_NEXT_DEFS)

rttree_prev_v.o:	rttree_prev_v.c $(RTTREE_NEXT_DEFS)
rttree_prev_c_v.o:	rttree_prev_c_v.c $(RTTREE_NEXT_DEFS)
rttree_prev_cx_v.o:	rttree_prev_cx_v.c $(RTTREE_NEXT_DEFS)
rttree_prev_x_v.o:	rttree_prev_x_v.c $(RTTREE_NEXT_DEFS)

rttree_prune_next.o:	rttree_prune_next.c $(RTTREE_NEXT_DEFS)
rttree_prune_next_c.o:	rttree_prune_next_c.c $(RTTREE_NEXT_DEFS)
rttree_prune_next_cx.o:	rttree_prune_next_cx.c $(RTTREE_NEXT_DEFS)
rttree_prune_next_x.o:	rttree_prune_next_x.c $(RTTREE_NEXT_DEFS)

rttree_prune_next_v.o:		rttree_prune_next_v.c $(RTTREE_NEXT_DEFS)
rttree_prune_next_c_v.o:	rttree_prune_next_c_v.c $(RTTREE_NEXT_DEFS)
rttree_prune_next_cx_v.o:	rttree_prune_next_cx_v.c $(RTTREE_NEXT_DEFS)
rttree_prune_next_x_v.o:	rttree_prune_next_x_v.c $(RTTREE_NEXT_DEFS)

rttree_prune_prev.o:	rttree_prune_prev.c $(RTTREE_NEXT_DEFS)
rttree_prune_prev_c.o:	rttree_prune_prev_c.c $(RTTREE_NEXT_DEFS)
rttree_prune_prev_cx.o:	rttree_prune_prev_cx.c $(RTTREE_NEXT_DEFS)
rttree_prune_prev_x.o:	rttree_prune_prev_x.c $(RTTREE_NEXT_DEFS)

rttree_prune_prev_v.o:		rttree_prune_prev_v.c $(RTTREE_NEXT_DEFS)
rttree_prune_prev_c_v.o:	rttree_prune_prev_c_v.c $(RTTREE_NEXT_DEFS)
rttree_prune_prev_cx_v.o:	rttree_prune_prev_cx_v.c $(RTTREE_NEXT_DEFS)
rttree_prune_prev_x_v.o:	rttree_prune_prev_x_v.c $(RTTREE_NEXT_DEFS)

rttree_release.o:	rttree_release.c $(RTTREE_DEFS)

rttree_sanity.o:	rttree_sanity.c rttree_sanity.h $(RTTREE_DEFS)
rttree_stats_init.o:	rttree_stats_init.c rttree.h

rttree_sub_first.o:	rttree_sub_first.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_first_c.o:	rttree_sub_first_c.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_first_cx.o:	rttree_sub_first_cx.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_first_x.o:	rttree_sub_first_x.c $(RTTREE_SUBTREE_DEFS)

rttree_sub_first_v.o:	rttree_sub_first_v.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_first_c_v.o:	rttree_sub_first_c_v.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_first_cx_v.o:	rttree_sub_first_cx_v.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_first_x_v.o:	rttree_sub_first_x_v.c $(RTTREE_SUBTREE_DEFS)

rttree_sub_last.o:	rttree_sub_last.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_last_c.o:	rttree_sub_last_c.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_last_cx.o:	rttree_sub_last_cx.c $(RTTREE_SUBTREE_DEFS)
rttree_sub_last_x.o:	rttree_sub_last_x.c $(RTTREE_SUBTREE_DEFS)

rttree_sub_next.o:	rttree_sub_next.c $(RTTREE_NEXT_DEFS)
rttree_sub_next_c.o:	rttree_sub_next_c.c $(RTTREE_NEXT_DEFS)
rttree_sub_next_cx.o:	rttree_sub_next_cx.c $(RTTREE_NEXT_DEFS)
rttree_sub_next_x.o:	rttree_sub_next_x.c $(RTTREE_NEXT_DEFS)

rttree_sub_next_v.o:	rttree_sub_next_v.c $(RTTREE_NEXT_DEFS)
rttree_sub_next_c_v.o:	rttree_sub_next_c_v.c $(RTTREE_NEXT_DEFS)
rttree_sub_next_cx_v.o:	rttree_sub_next_cx_v.c $(RTTREE_NEXT_DEFS)
rttree_sub_next_x_v.o:	rttree_sub_next_x_v.c $(RTTREE_NEXT_DEFS)

rttree_sub_prev.o:	rttree_sub_prev.c $(RTTREE_NEXT_DEFS)
rttree_sub_prev_c.o:	rttree_sub_prev_c.c $(RTTREE_NEXT_DEFS)
rttree_sub_prev_cx.o:	rttree_sub_prev_cx.c $(RTTREE_NEXT_DEFS)
rttree_sub_prev_x.o:	rttree_sub_prev_x.c $(RTTREE_NEXT_DEFS)

rttree_sub_prev_v.o:	rttree_sub_prev_v.c $(RTTREE_NEXT_DEFS)
rttree_sub_prev_c_v.o:	rttree_sub_prev_c_v.c $(RTTREE_NEXT_DEFS)
rttree_sub_prev_cx_v.o:	rttree_sub_prev_cx_v.c $(RTTREE_NEXT_DEFS)
rttree_sub_prev_x_v.o:	rttree_sub_prev_x_v.c $(RTTREE_NEXT_DEFS)

rttree_tune.o:		rttree_tune.c rttree.h

rttree_update.o:	rttree_update.c $(RTTREE_UPDATE_DEFS)
