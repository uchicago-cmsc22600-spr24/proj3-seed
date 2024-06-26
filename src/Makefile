# Makefile for class project
#
# COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
# All rights reserved.
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2024
# University of Chicago
#
# targets:
#	make mlc	-- Build MiniML compiler and copy heap file to bin
#			   directory
#

SHELL =         /bin/sh
OS =            $(shell uname -s)

# we assume that sml (and thus ml-build is in the path)
#
ML_BUILD =	ml-build

HEAP_SUFFIX =	$(shell sml @SMLsuffix)

BINDIR =	../bin
LIBDIR =	../lib

CM_FILES =	$(wildcard */sources.cm)

COMMON_SRCS =	ast/ast.sml \
		ast/basis.sml \
		ast/data-con.sml \
		ast/dump-ast.sml \
		ast/exp.sml \
		ast/meta-var.sml \
		ast/ty-con.sml \
		ast/ty-var.sml \
		ast/type-rep.sml \
		ast/type.sml \
		ast/var.sml \
		bind-checker/binding.sml \
		bind-checker/context.sml \
		bind-tree/bind-basis.sml \
		bind-tree/bind-tree.sml \
		bind-tree/dump-bind-tree.sml \
		bind-tree/identifier-fn.sml \
		common/dump-util.sml \
		common/error.sml \
		common/options.sml \
		common/stamp.sml \
		driver/main.sml \
		parse-tree/dump-parse-tree.sml \
		parse-tree/op-names.sml \
		parse-tree/parse-tree.sml \
		parser/parser.sml \
		parser/mml.grm \
		parser/mml.lex \
		type-checker/chk-exp.sml \
		type-checker/chk-pat.sml \
		type-checker/chk-ty.sml \
		type-checker/context.sml \
		type-checker/coverage.sml \
		type-checker/id-props.sml \
		type-checker/type-checker.sml \
		type-checker/type-util.sml \
		type-checker/unify.sml

MLC_SRCS =	$(COMMON_SRCS)

.PHONY:		mlc
mlc:		mlc.$(HEAP_SUFFIX)
	mv mlc.$(HEAP_SUFFIX) $(BINDIR)

# build rule for compiler
mlc.$(HEAP_SUFFIX):	$(CM_FILES) $(MLC_SRCS)
	$(ML_BUILD) driver/sources.cm Main.main mlc

.PHONY:		clean
clean:
		rm -rf bin/*.$(HEAP_SUFFIX)
		rm -rf .cm */.cm
