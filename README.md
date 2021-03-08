### setup
Running/building emash from source requires [SBCL](http://sbcl.org/), [libcurl](https://curl.se/libcurl/), [cffi](https://common-lisp.net/project/cffi/) and [whirlog](https://github.com/codr7/whirlog).

Once everything is in place, building an executable in the current directory goes something like this:

```
$ sbcl --eval "(asdf:operate :build-op 'emash)"
```

A pre-built [binary](https://github.com/codr7/emash/blob/main/emash) is provided for macOS.

### add smtp
emash embraces the idea of multiple smtp accounts, add as many as you like.

All settings are stored in a database rooted in the current working directory.

```
hello
emash> add-smtp
e-mail: foo@gmail.com
host: smtp.gmail.com
port: 465
user: foo
password: secret
created smtp foo@gmail.com
make default [y]/n? y
foo@gmail.com is default smtp
```

### post e-mail
The message is posted as soon as an empty line is entered in the body.

```
emash> post
1) foo@gmail.com
from 1-1 [foo@gmail.com]:
to: bar@gmail.com
subject: testing
body:
1 2 3

posted
emash>
bye
```