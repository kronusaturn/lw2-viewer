# lw2-viewer
An alternative frontend for [LessWrong 2.0](https://www.lesserwrong.com/), with a focus on speed and usability.

* [Issue tracker](https://saturn.obormot.net/LW2Reader/Issues)

## Installation
### Linux/Unix

*This is the bare minimum to get the server running. To set up a full Common Lisp development environment, see [here](http://lisp-lang.org/learn/getting-started/)*

* Install `sbcl` and `lmdb` from apt or using your favorite method.  
e.g. `sudo apt install sbcl liblmdb0`

* Install [quicklisp](https://beta.quicklisp.org/)  
`curl -O https://beta.quicklisp.org/quicklisp.lisp`  
`sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --eval '(exit)'`  

* Clone required git repositories to the quicklisp `local-projects` directory. (You can also use symlinks if you want to put them elsewhere.)  
`cd ~/quicklisp/local-projects`  
`git clone https://github.com/kronusaturn/liblmdb.git`  
`git clone https://github.com/kronusaturn/lmdb.git`  
`git clone https://github.com/kronusaturn/plump.git`  
`git clone https://github.com/kronusaturn/lw2-viewer.git`

* Start the server  
`cd ~/quicklisp/local-projects/lw2-viewer`  
`sbcl --eval '(ql:quickload :lw2-viewer)' --eval '(hunchentoot:start (make-instance (quote hunchentoot:easy-acceptor) :port 4242 :document-root "./www/"))'`  

* Open `http://localhost:4242` in your browser

### Mac
Similar to above, but:
* `sbcl` and `lmdb` are available from MacPorts.

* Make sure to run system updates, using an outdated OS version can cause installation errors.

* As Mac OS does not support sparse files, the system will create an empty 16 GB file on startup.
If you want to reduce disk space usage, before starting the server, copy config-example.lisp to config.lisp and change the line `(defparameter *lmdb-mapsize* (expt 2 34))` to, for example, `(defparameter *lmdb-mapsize* (expt 2 28))`

* You will need to set the `DYLD_LIBRARY_PATH` environment variable to the location where `liblmdb.dylib` is installed, for example `export DYLD_LIBRARY_PATH=/opt/local/lib/`
