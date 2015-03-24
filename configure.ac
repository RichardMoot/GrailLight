AC_INIT(graillight, 0.9, Richard.Moot@labri.fr)

dnl Check for SWI Prolog/XPCE
AC_PATH_PROG(XPCE, [xpce], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(XPCE)
AC_PATH_PROG(SWIPL, [swipl], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(SWIPL)

dnl Check for Tclsh
AC_PATH_PROG(TCLSH, [tclsh], NULL, [path=$PATH$PATH_SEPARATOR/usr/bin$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(TCLSH)

dnl Check for mv
AC_PATH_PROG(MV, [mv], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(MV)

dnl Check for sed
AC_PATH_PROG(SED, [sed], NULL, [path=$PATH$PATH_SEPARATOR/usr/bin$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(SED)

dnl Check for pdf viewer
AC_PATH_PROGS(PDF, [Preview acrobat acroread gv evince], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin$PATH_SEPARATOR/Applications/Preview.app/Contents/MacOS/Preview])
AC_SUBST(PDF)

AC_SUBST(GRAIL_YEAR, `date +%Y`)

AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(bindir)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
