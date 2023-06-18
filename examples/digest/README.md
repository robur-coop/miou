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
examples/digest/main.ml: 64670d9a6b1e6d3639a9356d0ee583545db3187d83d975b535476a8c4a41e824
```

The program expects a list of files which can be given on the command line or
transmitted via standard input. The hash algorithm used is SHA256.
