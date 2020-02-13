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
./posixtime "/home/foo/Love.jpg"
"/home/lisper/Love.jpg"	 Access: 2019-10-08 21:46:52.423287501 UTC	 Modify: 2018-05-14 04:14:17.501787194 UTC	 Change: 2018-08-17 21:44:16.479672482 UTC

```

# In a shell script

```bash
# check directory
$ ~> cd dir-example
$ ~> ls
posixtime  posixtime-tmp

# loop
for i in $(ls -1)
do
  ./posixtime $i
done

# output

"posixtime"	 Access: 2020-02-13 05:31:11.199683948 UTC	 Modify: 2020-02-13 05:30:51.435884439 UTC	 Change: 2020-02-13 05:30:51.435884439 UTC
"posixtime-tmp"	 Access: 2020-02-13 05:30:51.023888604 UTC	 Modify: 2020-02-13 05:30:50.999888846 UTC	 Change: 2020-02-13 05:30:50.999888846 UTC

```
