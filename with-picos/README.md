# Miou × [Picos][picos]

This part of the distribution wants to allow users to combine Miou with a
program which uses basic effects from Picos. Miou is essentially based on the
`Picos.Trigger.Await` effect. This is the basic suspension effect used by
`Picos.Computation.t`. Any other effects that Picos exposes are **not** handled
by Miou (you need to install a handler on top of Miou that handles them).

The user is able to transparently switch from `miou` to `miou-with-picos`: we
keep the same interface. However, it is **not** possible to co-use both (both
offer the Miou module). This library is an experiment which requires that for
large projects, you make the change by hand if you wish, at this stage, to
interoperate with other libraries using the `Await` effect.

The idea of this specific library is to be able to interoperate between several
libraries sharing the same effect in **an experimental way** until the latter
(the effect) is consensually accepted by all. As such, we do not support or
distribute this library to the community (in this case, a [pin][pin] is
required).

[picos]: https://github.com/ocaml-multicore/picos
[pin]: https://opam.ocaml.org/doc/Usage.html#opam-pin
