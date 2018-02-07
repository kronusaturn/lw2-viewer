# lw2-viewer
An alternative frontend for [LessWrong 2.0](https://www.lesserwrong.com/), with a focus on speed and usability.

* [Issue tracker](https://saturn.obormot.net/LW2Reader/Issues)

## Installation
* Install `sbcl` and `lmdb` from apt, macports, homebrew, etc.  
e.g. `sudo apt install sbcl liblmdb0`

* Install [quicklisp](https://beta.quicklisp.org/)  
`curl -O https://beta.quicklisp.org/quicklisp.lisp`  
`sbcl --load quicklisp.lisp`  

* Clone required git repositories to the quicklisp `local-projects` directory. (You can also use symlinks if you want to put them elsewhere.)  
`cd ~/quicklisp/local-projects`  
`git clone https://github.com/kronusaturn/lmdb.git`  
`git clone https://github.com/kronusaturn/plump.git`  
`git clone https://github.com/kronusaturn/lw2-viewer.git`

* Start the server  
`cd ~/quicklisp/local-projects/lw2-viewer`  
`sbcl --eval '(ql:quickload :lw2-viewer)' --eval '(hunchentoot:start (make-instance (quote hunchentoot:easy-acceptor) :port 4242 :document-root "./www/"))'`  

* Open `http://localhost:4242` in your browser
