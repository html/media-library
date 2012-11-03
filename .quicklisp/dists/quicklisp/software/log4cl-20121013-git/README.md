# Quickstart
## Installation and loading

### From [QuickLisp](http://www.quicklisp.org) 

LOG4CL is now included in [QuickLisp](http://www.quicklisp.org). To
load it use the command `(ql:quickload :log4cl)` from `REPL`. If
QuickLisp can't find it, you need to update your QuickLisp
distribution.

### Latest GIT version

To use the latest GIT version, which may contain improvements and bug
fixes not yet available in [QuickLisp](http://www.quicklisp.org), you
can use [QuickLisp's](http://www.quicklisp.org) local-projects feature.

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/7max/log4cl.git 

Then use the `(ql:quickload :log4cl)` from `REPL` to load it.

### Log4CL packages

Do not try to include LOG4CL into your package's *:USE* list, instead use
`log:` package prefix with your log statements.

## The Basics

     (log:info "Hello World")

         [14:39:03] [info] <cl-user> - Hello World

     (log:i "I'm really lazy")

         [14:39:08] [info] <cl-user> - I'm really lazy

     (log:debug "something")

As you can see package contains one letter shortcuts for all the log
levels, for the lazy amongst us. Also note that debug log statement
did not produce any output. Changing the log level is easy:

    (log:config :debug)

    (log:debug "something")

    [14:41:44] [debug] <cl-user> - something

As with the log statements, you can abbreviate `config` and
`:debug` to one letter, for example: `(log:c :d)`

One additional logging utility function `(log:expr)`, helps you to
quickly debug values of variables and expressions:

    (let ((a 1) (b 2))
      (log:expr a b (+ a b)))

    [14:54:39] [debug] <cl-user> - A=1 B=2 (+ A B)=3

It function also has two aliases: `(log:sexp)` and `(log:s)`. For completeness
there are also `(log:sexp-trace)`, `(log:sexp-info)` and so on, to allow for
same expression logging with any log level

### Log severity levels 
LOG4CL has all the same log levels that Log4J does, and some
additional ones. They are in the order of decreasing severity:
*fatal*, *error*, *warn*, *info*, *debug*, *user1* through *user4*,
*trace*, and finally *user5* through *user9*

There is also two special log levels *off* and *unset*, which 
turn the logging off, and inherit the log level from the parent
logger. The default log level for all loggers is *unset*.

When a logging is on for a log level, it is also enabled for levels
with more severity. For example if *debug* level is on, then *info*
and *error* are also on.

### How to determine if logging is on programatically?

Sometimes its useful to conditionally execute log statements. The
logging macro without any arguments, will return `T` if logging is on:

    (when (log:debug)
      (print-detailed-debugging-info))

## Loggers and categories

You may have noticed that the log output in addition to the time of
the log message, its severity level and the actual log message,
contains the string `<cl-user>`, which corresponds to the current
package.

That string is the category of the log message, and is also referred
to as *logger's category*, or simply a logger.

*Logger* is an object, that acts as source of the log message, and
also as a configuration point, which can have its own log level and
appenders. Log messages are always logged into a specific logger
object.

Loggers form a hierarchy, starting from the root logger. Each logger
can have child loggers, and these loggers can have their own child loggers and
so on. The full category name of the logger, is concatenation
of all the parent logger names starting from the root logger, with some
separator string, which defaults to `":"`

So for a logger `A` which has a child logger `B`, which in turn has
child logger `C`, the full category name of the logger C is `A:B:C`

### Automatic naming of loggers

When you issue a log statement such as `(log:info "whatever")` LOG4CL
determines the logger name automatically based on surrounding context.
That's why, when you issue log statement from `REPL` with the current
package being *CL-USER*, the logger message went to was also called
`CL-USER`.

You can directly instantiate logger objects, by using
the `(log:make-logger)` function:

    (log:make-logger)

    #<LOGGER CL-USER>

    (log:make-logger :a)

    #<LOGGER CL-USER:A>

    (log:make-logger '(one two three))

    #<LOGGER ONE:TWO:THREE>

In the first example, the logger name is automatically determined from
context, as with the default log statement. In the second
example, specifying a keyword as the logger name, will create a
sub-logger of such default logger. And finally the most generic syntax
is using a quoted list, where each element names a logger starting
from root.

When submitting a log message, you can specify a logger object as a
first argument:

    (log:info (log:make-logger :a) "goes to logger A")

    [15:14:51] [info] <cl-user:a> - goes to logger A

    (log:info (log:make-logger '(one two three)) "goes to logger ONE:TWO:THREE")

    [15:15:04] [info] <one:two:three> - goes to logger ONE:TWO:THREE

As a shortcut you can omit the `(log:make-logger`) and specify a
`(make-logger)` argument directly as a first argument of a logging
function, with exception of `(log:expr)` and friends.

    (log:info '(one two three) "goes to logger ONE:TWO:THREE")

    [15:18:02] [info] <one:two:three> - goes to logger ONE:TWO:THREE

    (log:info :b "goes to logger CL-USER:B")

    [15:18:15] [info] <cl-user:b> - goes to logger CL-USER:B

### Automatic naming inside a function

Under SBCL automatic logger naming goes farther then naming
the logger after the current package:

    (defun foo (x)
      (log:expr x (1+ x))) 

    (foo 3)

    [16:47:09] [debug] <cl-user:foo> - X=3 (1+ X)=4

    (defun foo (x)
      (flet ((baz ()
               (log:expr x (1+ x))))
        (baz)))

    (foo 3)

    [16:47:09] [debug] <cl-user:foo:baz> - X=3 (1+ X)=4

It also handles methods:

    (defmethod bar ((x number) y)
      (flet ((baz ()
        (log:expr x (1+ x) y)))
           (baz)))

    (bar 3 'foo)

    [16:52:15] [debug] <cl-user:bar:number:baz> - X=3 (1+ X)=4 Y=FOO

    (defmethod bar :before (x y) (log:info "what happens before ~s ~s" x y))

    (bar 41 t)

  
    [16:55:11] [info] <cl-user:bar:before> - what happens before 41 T
    [16:55:11] [debug] <cl-user:bar:number:baz> - X=41 (1+ X)=42 Y=T

### Configuring a specific logger category

By default a call to `(log:config)` function configures the root
logger. All the rest of the loggers in the system inherit the log
level from their parent loggers, and eventually from the root logger.

It is possible to configure each logger to have its own log level, which
will override the one inherited from the parent:

    (log-config '(cl-user a) :debug)
    (log-config '(cl-user b) :trace)
    (log-config '(cl-user) :info)

    (log:info "this is info")

    [15:26:50] [info] <cl-user> - this is info

    (log:debug "this is debug")

    (log:debug :a "this is debug")

    [15:27:05] [debug] <cl-user:a> - this is debug

    (log:trace :a "this is trace")

    (log:trace :b "this is trace")

    [15:27:22] [trace] <cl-user:b> - this is trace   
    
In the above example we had configured logger `CL-USER` `CL-USER:A`
and `CL-USER:B` with the *info*, *debug* and *trace* log levels, and
tested submitting log messages to them. To make logger inherit its log
level from its parent again, use the log level *unset* or use the
*:clear* option of the configuration function.

    (log:config '(cl-user) :unset)

    (log:debug "test")

    [15:34:37] [debug] <cl-user> - test

    (log:config '(cl-user) :info :clear)

    (log:debug :a "this is debug)

Note: *:clear* option resets the log level of all the child loggers of
the specified logger, but not the logger itself. To do both use
`(log:config '(logger) :unset :clear)`

### Displaying logger hierarchy configuration

Without any arguments, `(log:config)` will display logging hierarchy
and its configuration on the standard output. It only prints
"interesting" loggers, that have either an explicit log level, have
any appenders, or are non-additive (meaning they don't propagate
messages to the ancestor's appenders)

    CL-USER> (log:config)
    ROOT, DEBUG
    |
    +-#<CONSOLE-APPENDER {102576AD21}>
    |     with #<PATTERN-LAYOUT {102566A5E1}>
    |          :conversion-pattern "[%D{%H:%M:%S}] [%P] <%c{}{}{:downcase}> - %m%n"
    |     :immediate-flush NIL
    |     :flush-interval  1
    |
    +-LOG4CL-IMPL
      |
      +-SELF (non-additive), WARN
        |
        +-#<CONSOLE-APPENDER {101EAEB6B1}>
              with #<PATTERN-LAYOUT {101E55FC21}>
                   :conversion-pattern "[%d{%H:%M:%S}] [%-5P] <%c{}{}{:downcase}>%n  *%I{>} %m%n"
              :immediate-flush NIL
              :flush-interval  1
    CL-USER> 

## Appenders and layouts

While loggers are logical sinks for the log messages, appenders are
physical ones, that are responsible for formatting the log messages
and delivering them to their ultimate destination.

For example `CONSOLE-APPENDER` writes message to `*DEBUG-IO*` stream
and `DAILY-FILE-APPENDER` writes messages into a file that rolls over
each day.

### Layouts

Appenders format the log messages by means of layouts. Layout receives
information about logging event, and the stream to print output into,
and is responsible for actual formatting of the message, such as
adding timestamps or other formatting.

LOG4CL provides two layout classes:

- `SIMPLE-LAYOUT` formats the message in a very primitive way, by
  printing the log level followed by a dash and the user log message

- `PATTERN-LAYOUT` formats the message by specifying a conversion
  pattern string, which has escape sequences for various part of the
  log message. See Lisp docstring for `PATTERN-LAYOUT` class for the
  description of the pattern layout elements.

### Configuring appenders and layouts with `(log:config)` function

The versatile `(log:config)` function can also add and remove
appenders and their layouts. See the Lisp docstring for full
description, below are a few examples:

- `:SANE` option removes all appenders from the logger, and adds a
  `LOG:CONSOLE-APPENDER` with a pattern layout used in throughout the
  examples in this document

            (log:config :sane)

            (log:info "test")

            [15:54:04] [info] <cl-user> - test

- `:TWOLINE` option changes the pattern layout used to have output on
  two lines, the header line with time-stamp, log level, and logger's
  category, and second line with the actual log message.

              (log:config :sane :twoline)

              (log:info "test")

              [15:56:14] [ info] <cl-user>
                * test


- `:DAILY` option adds a `LOG:DAILY-FILE-APPENDER`, logging into the
  specified file. The file will switch at midnight, with the old file
  renamed by adding `<year><mon><day>` suffix to its name.

              (log:config :daily "log.txt")

              (log:info "test")

              [15:56:14] [ info] <cl-user>
                * test

              $ cat log.txt
              [15:58:50] [info] <cl-user> - test
  It will use same pattern layout as the *:sane* option, it will also
  accept *:twoline*. If both *:sane* and *:daily* are used, then
  console appender is not added, you can use *:console* option to
  force adding both file and console appenders.

- `:PATTERN` option allows one to change the conversion pattern for
  the new pattern layout used with new appenders added by *:sane*,
  *:daily* and/or *:console* options

              (log:config :sane :pattern "%d -- %p -- %m%n")

              (log:info "test")

              2012-02-23 21:05:12 -- INFO -- test

- `:PROPERTIES` option configures logging system from Log4J style
  properties file. There are important differences from Log4J:

  - Logging configuration lines start with log4cl instead of log4j

  - The default separator is colon instead of dot, similar to log
    category names

              $ cat tests/log4cl.properties

              log4cl:rootLogger = INFO, file1, console
              log4cl:appender:console = log4cl:console-appender
              log4cl:appender:console:layout = log4cl:pattern-layout
              log4cl:appender:console:layout:conversion-pattern =    |%p| |%c| - %m%n
              log4cl:appender:file1 = log4cl:file-appender
              log4cl:appender:file1:file = /tmp/logfile.txt
              log4cl:appender:file1:immediate-flush = true
              
              (log:config :properties "tests/log4cl.properties")

              (log:info "testing")

                  |INFO| |CL-USER| - testing

              $ cat /tmp/logfile.txt

              INFO - testing
  Note that whitespace is not stripped from the start of conversion
  pattern property, but is stripped from the beginning of the
  file-name property.

- `:WATCH` option together with *:PROPERTIES* option, will make LOG4CL
  watch the file modification time, and reload it when it changes.

              (log:config :properties "tests/log4cl.properties" :watch)

              (log:info "testing")

                  |INFO| |CL-USER| - testing

              $ cat /tmp/logfile.txt

              INFO - testing
  Now modify and save the *log4cl.properites* file.

              |INFO| |LOG4CL| - Re-configured logging from tests/log4cl.properties
  Now modify the file to have some error
   
              |ERROR| |LOG4CL| - Re-configuring from tests/log4cl.properties failed:
              "log4cl:appender:console:layout:conversion-patter =    |%p| |%c| - %m%n"
              Error at line 6:
              Unknown property :CONVERSION-PATTER for class #<PATTERN-LAYOUT
                                                              {10761F4841}>

## Quick save/restore

  Once you added extensive logging to the your application, it may become bothersome
  to reconfigure log levels each time you work on a separate part of a big system.

  For example, when focusing one module A, you need detailed debugging for it, but
  when focusing on module B, the debug statements coming from A flooding your screen
  are unhelpful. Of course one can use separate log4cl.properties file for each part
  of theh system that you are developing, or have a separate .lisp
  file with `(log:config)` statemements, but LOG4CL now provides a more intuitive
  and agile facility.

    LTR> (log:save :config-1)

    #<log4cl-impl:logging-configuration :config-1 (18)>

    LTR> (log:config :clear :info)
    LTR> (log:save)

    #<log4cl-impl:configuration "Saved on 2012-07-01 12:51:02" (1)>

    LTR> (log:restore)

    #<log4cl-impl:configuration :config-1 (18)>
    
    LTR> (log:restore)

    #<log4cl-impl:configuration "Saved on 2012-07-01 11:44:18" (1)>

    LTR> (log:list-configurations)
      0. #<CONFIGURATION "Saved on 2012-07-01 11:44:18" (1)>
      1. #<CONFIGURATION :CONFIG-1 (18)>

    (log:restore 1)

    #<log4cl-impl:configuration :config-1 (18)>

    LTR> (log:config '(ltr scaling.lisp) :trace)
    LTR> (log:save :config2)

    #<log4cl-impl:configuration :config2 (19)>

    LTR> (log:list-configurations)
      0. #<CONFIGURATION :CONFIG2 (19)>
      1. #<CONFIGURATION :CONFIG-1 (18)>
      2. #<CONFIGURATION "Saved on 2012-07-01 11:44:18" (1)>

The `(log:save)` function saves the logging configuration
and `(log:restore)` restores last saved configuration.. There are up to 30
configuration saved, and they are automatically persisted into the
`~/.log4cl-configurations.lisp-expr` file, so they survive lisp
restarts.  When restore function notices that current configuration is
not in the configuration list, it will automatically create a
"Autosave on <timestamp>" configuration, so you can recover from
accidents.

`log:save` and `log:restore` are also aliased to `log:push` and `log:pop`.

## More documentation

  Hopefully this Quick-Start guide covered enough to get you up and running, and
  using basic logging in your application.

  Description of the more advanced features, such as logging
  hierarchies, and how to customize LOG4CL by adding your own
  appenders and layouts, or customizing auto-naming will be covered
  later once user manual is finished.

  For now, you can browse the source code, where most generic
  functions, classes and methods have detailed docstrings. When doing
  so, please note that *LOG4CL* is simply a forwarder package, intended
  to be used only for log statements, and not for any customizations;
  the actual implementation of *LOG4CL* lives in the *LOG4CL-IMPL*
  package, which exports everything needed to for writing your own
  appenders and layouts. You can start with `src/appender.lisp` if you
  are looking for examples.

