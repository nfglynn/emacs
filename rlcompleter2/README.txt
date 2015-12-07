=============================================================
rlcompleter2 0.96, interactive python command line completion 
=============================================================

.. contents::
.. sectnum::

download: `rlcompleter2-0.96.tar.gz`_,  ``(c) 2003-2005 Holger Krekel, hpk@trillke.net, license: MIT-License``

.. _`rlcompleter2-0.96.tar.gz`: http://codespeak.net/rlcompleter2/download/rlcompleter2-0.96.tar.gz 

introduction 
============ 

One of the best features of Python_ is that you can use and
learn it interactively.  *rlcompleter2* enhances this
interactive experience.  It is a major improvement over the
'rlcompleter' in the standard library.

I recommend that you simply install it and see if you 
like it.  The user interface is simple: hit <tab> one
or more times.  If you sometimes use python interactively you 
will certainly enjoy it.  If you don't work interactively, 
well, then you should :-)

.. _Python: http://www.python.org 

features
-------- 

- completion on any valid Python statement/expression

- many convenient completions, e.g.

  func(<tab>   presents function signatures and docs
  module.<tab> presents module docstring
  func<tab>    completes '(' or '()' 
  obj<tab>     completes '.' if obj is module or instance

- when you hit <tab> multiple times you will get 
  increasing details about multiple completions

- the completer tries to autodetect your terminal height
  and width.

- shows type information for multiple matches. types
  are abbreviated by three characters.
  
- customaizable via the config class which you can inherit
  Look at the source at the bottom to get a clue.


Installation 
============

The quick start way 
-------------------

0. requirements: 

   - readline module (``import readline`` shouldn't fail)
   - Python-2.2 or later 

1. download `rlcompleter2-0.96.tar.gz`_ 

2. type into some shell window:: 

       tar zxf rlcompleter2-0.96.tar.gz 
       cd rlcompleter2-0.96 
       python -i rlcompleter2 

2. now every time you want completion/information in some 
   expression hit <tab> one or multiple times.  I recommend starting 
   with importing the 'os' module and typing 'os.get<tab>'

Permanent Install (with distutils)
---------------------------------- 

There is only one module, ``rlcompleter2.py``, which needs to be 
in your python path but for your convenience you can use distutils:

1. type:: 

       python setup.py install 

   this installs rlcompleter2.py into your site-packages directory.

2. set your environment variable ``PYTHONSTARTUP`` to a file 
   which contains:: 

       import rlcompleter2
       rlcompleter2.setup()

   note that you probably want to set ``PYTHONSTARTUP`` permanently 
   in your shell startup (e.g. `.bash_profile` for bash). 

3. now you simply invoke "python" and the completer will be there:: 

     $ python 
     Python 2.3.5 (#1, Mar  2 2005, 08:59:42) 
     [GCC 3.4.3-20050110 (Gentoo Linux 3.4.3.20050110, ...)] 
     Type "help", "copyright", "credits" or "license" for more information.
     Welcome to rlcompleter2 0.96
     for nice experiences hit <tab> multiple times
     >>>


Notes and the future
====================

how the interactive completer works
----------------------------------- 

rlcompleter2 works as a callback object for the readline 
library. It doesn't just give you completions on much 
more expressions than the stdlib-rlcompleter. It also 
gives you incrementally more introspective information 
the more often you hit <tab>.  

The completer aims to be very intuitive and helpful.
You can customize behaviour via the Config class 
which is instantiated per Completer (to enable it 
to be multiply embedded with e.g. IPython)

Feel free to suggest better default behaviour or
configuration options! 

We can't escape the Heisenberg problem
--------------------------------------

Anytime you hit <TAB> you may *change* objects
in your environment. This is because the completer 
not only parses the input but also partially compiles 
and evaluates it.  Python is dynamically typed so
we can't infer type/class information with only
parsing.  But evaluation of code can have 
side effects, if it involves e.g. a ``file.read()`` method.  
In interactive command line practice this is seldomly a 
big problem, though. 

Future and More Information 
--------------------------- 

For most up to date information please look at 
http://codespeak.net/rlcompleter2/

There is a rewrite underway which aims to get more of the
completion machinery tested.  The completer really stems from
a time where i wasn't enlightned enough to realize that
testing really shortens overall development time and makes 
it more effective and fun.    

Moreover, it is very likely that rlcompleter2's functionality 
will be included in `the py lib`_. 

.. _`the py lib`: http://codespeak.net/py 

