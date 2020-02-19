# Posixtime
This binary return the __access time__, __modification time__ and __status change time__ of a file.

# Usage

```bash
# building
$ ~> git clone https://github.com/innaky/haskell-misc.git
$ ~> cd haskel-misc/learn-you-a-haskell/io/posixtime
$ ~> cabal update
$ ~> cabal new-build

# Exec
./posixtime local
Input the path: 
/home/live
("/home/live/src","Wed Feb 19 09:40:05 2020","Wed Feb 19 09:39:18 2020","Wed Feb 19 09:39:18 2020")
("/home/live/.bashrc","Tue Feb 18 23:15:54 2020","Sun Feb 16 17:02:30 2020","Sun Feb 16 17:02:30 2020")
("/home/live/.config","Wed Feb 19 07:31:15 2020","Wed Feb 19 09:38:53 2020","Wed Feb 19 09:38:53 2020")
("/home/live/.gitconfig","Wed Feb 19 07:15:00 2020","Mon Feb 17 03:10:13 2020","Mon Feb 17 03:10:13 2020")
("/home/live/quicklisp.lisp","Sun Feb 16 23:04:09 2020","Sun Feb 16 23:03:28 2020","Sun Feb 16 23:03:28 2020")
("/home/live/.xmonad","Wed Feb 19 07:31:15 2020","Sun Feb 16 21:02:23 2020","Sun Feb 16 21:02:23 2020")
("/home/live/quicklisp.lisp.asc","Sun Feb 16 23:04:09 2020","Sun Feb 16 23:03:57 2020","Sun Feb 16 23:03:57 2020")
("/home/live/Downloads","Wed Feb 19 07:31:15 2020","Mon Feb 17 22:35:48 2020","Mon Feb 17 22:35:48 2020")
("/home/live/quicklisp","Wed Feb 19 07:31:15 2020","Sun Feb 16 23:40:27 2020","Sun Feb 16 23:40:27 2020")
("/home/live/.cabal","Wed Feb 19 07:31:15 2020","Mon Feb 17 00:30:36 2020","Mon Feb 17 00:30:36 2020")
("/home/live/isos","Wed Feb 19 07:31:15 2020","Mon Feb 17 00:51:41 2020","Mon Feb 17 00:51:41 2020")
("/home/live/.emacs.d","Wed Feb 19 07:31:15 2020","Sun Feb 16 23:52:32 2020","Sun Feb 16 23:52:32 2020")
("/home/live/books","Wed Feb 19 07:31:15 2020","Sun Feb 16 22:47:11 2020","Sun Feb 16 22:47:10 2020")
("/home/live/.slime","Wed Feb 19 07:31:15 2020","Sun Feb 16 23:40:22 2020","Sun Feb 16 23:40:22 2020")
("/home/live/.ghc","Wed Feb 19 07:31:15 2020","Sun Feb 16 22:14:44 2020","Sun Feb 16 22:14:44 2020")
("/home/live/.local","Wed Feb 19 07:31:15 2020","Mon Feb 17 03:37:55 2020","Mon Feb 17 03:37:55 2020")
("/home/live/.profile","Mon Feb 17 05:00:06 2020","Sun Feb 16 17:02:30 2020","Sun Feb 16 17:02:30 2020")
("/home/live/.Xauthority-n","Wed Feb 19 00:20:32 2020","Wed Feb 19 00:20:32 2020","Wed Feb 19 00:20:32 2020")
("/home/live/.gnupg","Wed Feb 19 07:31:15 2020","Mon Feb 17 02:27:16 2020","Mon Feb 17 02:27:16 2020")
("/home/live/.emacs~","Sun Feb 16 21:07:39 2020","Sun Feb 16 21:07:58 2020","Sun Feb 16 21:07:39 2020")
("/home/live/.sbclrc","Sun Feb 16 23:43:19 2020","Sun Feb 16 23:40:47 2020","Sun Feb 16 23:40:47 2020")
("/home/live/.bash_logout","Sun Feb 16 17:02:30 2020","Sun Feb 16 17:02:30 2020","Sun Feb 16 17:02:30 2020")
("/home/live/.xsession-errors","Wed Feb 19 00:20:32 2020","Wed Feb 19 09:39:13 2020","Wed Feb 19 09:39:13 2020")
("/home/live/.slime-history.eld","Mon Feb 17 00:01:36 2020","Mon Feb 17 00:01:36 2020","Mon Feb 17 00:01:36 2020")
("/home/live/.bash_history","Wed Feb 19 07:13:49 2020","Wed Feb 19 07:13:49 2020","Wed Feb 19 00:56:27 2020")
("/home/live/.cache","Wed Feb 19 07:31:15 2020","Tue Feb 18 05:30:13 2020","Tue Feb 18 05:30:13 2020")
("/home/live/.emacs","Tue Feb 18 01:44:40 2020","Sun Feb 16 23:43:11 2020","Sun Feb 16 23:43:11 2020")
("/home/live/.mozilla","Wed Feb 19 07:31:15 2020","Sun Feb 16 22:42:22 2020","Sun Feb 16 22:42:22 2020")
("/home/live/sbcl-2.0.1-x86-64-linux","Wed Feb 19 07:31:15 2020","Sun Feb 16 23:36:45 2020","Sun Feb 16 23:36:45 2020")
("/home/live/.ssh","Wed Feb 19 07:31:15 2020","Sun Feb 16 23:49:10 2020","Sun Feb 16 23:49:10 2020")
```

