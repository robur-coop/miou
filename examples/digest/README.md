# Digest files

This small example shows a program that parallels the calculation of file hashes
but also limits the number of possible domains to allocate. This number is
limited to 2: this means that only 2 domains can coexist. If a domain is
requested again, it will have to wait until at least one of the two domains has
finished.

Miou does not limit the number of domains you can allocate, but it can be useful
to limit this number and define an allocation policy according to what you want
to do. `Domain.recommended_domain_count` gives you a limit based on your
resources (the number of cpu you have). You can run the programme in this way:
```shell-session
$ dune exec examples/digest/main.exe -- examples/digest/main.ml
examples/digest/main.ml: 28cfc08ddef2d9f0bd78bdd2d5e5f5e33d77f1eab5d8d61d482cbd18df9adb87
```

The program expects a list of files which can be given on the command line or
transmitted via standard input. The hash algorithm used is SHA256.
