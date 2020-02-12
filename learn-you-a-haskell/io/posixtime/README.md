# Posixtime
This binary return the Access time, modification time and status change time of a file.

# Usage

```bash
cabal update
cabal new-build
./posixtime "/home/foo/file"
(1581475695,1581475692,1581475692)

# Other examples

for i in $(ls -1)
do
  ./posixtime $i
done

(1581476088,1581475944,1581475944)
(1581475944,1581475944,1581475944)

```
