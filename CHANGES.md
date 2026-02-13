### v0.5.3 (2026-02-13)

- Fix how we handle signals and transfer them to the `dom0` (#98, @dinosaure,
  reported by @voodoos)

### v0.5.2 (2026-01-26)

- Add the Windows support (from our old `Unix.select` implementation)
  (@dinosaure, #94)
- Use `-Werror` only on the released mode (@dinosaure, #93)

### v0.5.1 (2026-01-12)

- Fix a deadlock when we fall into an abnormal situation (when we broke rules)
  (@dinosaure, #84)
- Add `POLLHUP` event when we check if a file-descriptor is read-ready
  (specially for pipe) (@madroach, @dinosaure, #89)

### v0.5.0 (2025-10-13)

- Use `poll(2)`/`ppoll(2)` instead of `select(3P)` (#75, @dinosaure, @haesbaert,
  @hannesm, @backtracking)

  `miou.unix` now uses the `poll(2)` or `ppoll(2)` function if available (the
  choice is determined at compilation). It replaces the use of `select(3P)` and
  improves performance. Miou no longer needs to build lists of file descriptors
  to observe, but instead manipulates a bitv and an array containing these file
  descriptors.

  The `bitv` implementation comes from the [bitv][bitv] library written by
  @backtracking, who kindly allowed us to relicense it under MIT.

  Finally, a special thanks to @haesbaert, who originally wrote
  [ocaml-iomux][iomux], which provides a portable implementation and an OCaml
  interface for using `poll(2)`/`ppoll(2)`.

  The use of `poll(2)`/`ppoll(2)` should improve performance, as noted in the
  PR, particularly with regard to our [httpcats][httpcats] HTTP server.

  It should be noted that `dune-configurator` has been added as a new dependency
  for Miou. However, support for `topkg`/`ocamlbuild` is still maintained (and
  it is possible to compile and install Miou with this build system).

- Correctly clean-up internals structures used by domains when we call multiple
  times `Miou_unix.run` and clean-up cancelled file-descriptors
  (#82, @dinosaure)

- Synchronize `dom0` when one domain receive a signal (#78, @omegametabroccolo,
  @dinosaure, @reynir, partially fix #77)

- Add `Miou.Ownership.release` to disown and release a resource
  (@dinosaure, #79)

[bitv]: https://github.com/backtracking/bitv
[ocaml-iomux]: https://github.com/ocaml-multicore/ocaml-iomux
[httpcats]: https://github.com/robur-coop/httpcats

### v0.4.0 (2025-08-06)

- Fix the suspension mechanism and allow the user to pass a function which will
  be executed when the suspension is confirmed by Miou (@dinosaure, #58)
- Clean-up correctly cancelled syscalls (@dinosaure, #59)
- Fix the number of orphans we count (@dinosaure, #60)
- Do some micro-optimisations when we use only one core (@dinosaure, #61)
- Add tests about `waitpid` and document it (@dinosaure, @mbarbin, #64, #66)
- Explain into the documentation the Miou behavior about exceptions
  (@kit-ty-kate, @dinosaure, #67)
- Update the README.md and add a CODE_OF_CONDUCT.md (@dinosaure, #68)
- Fix our tests on Windows (@dinosaure, #69)

### v0.3.1 (2025-01-13)

- Notice the domain if it needs to look into the shared heap if a task is ready
  to be transfered (@dinosaure, [#41][pr41])
- Don't use `Option.value` but `match .. with` to calculate the optional length
  for `Miou_unix.{read,write}` (@kit-ty-kate, [#44][pr44])
- Use `List.iter` instead of `Hashtbl.iter` for internal kept file-descriptors
  of `Miou_unix` (@dinosaure, [#45][pr45])
- Improve the documentation of `Miou_unix` about suspended syscalls (@dinosaure,
  @kit-ty-kate, [#43][pr43])
- Export `reraise` (@dinosaure, [#46][pr46])
- Fix an issue on the `dom0` and observe if some tasks must be transfered to it
  (@dinosaure, [#48][pr48])
- Fix documentation (@benjamin-thomas, [#47][pr47])
- Fix the formatter (@mbarbin, [#51][pr51])
- Upgrade miou to `ocamlformat.0.27.0` (@mbarbin, [#52][pr52])
- Add `x-maintenance-intent` (@hannesm, [#56][pr56])
- Improve the documentation and some `odoc` warnings (@mbarbin, [#53][pr53],
  [#54][pr54])

[pr41]: https://github.com/robur-coop/miou/commit/e10c048c989469d6456dd0fda4e23cf53c36d243
[pr44]: https://github.com/robur-coop/miou/commit/2309662efb633f3c50cbd5f0f57bf97d877221dd
[pr45]: https://github.com/robur-coop/miou/commit/750255cae4c791d07571596b6add584e2fd13580
[pr43]: https://github.com/robur-coop/miou/commit/98b4ee000e1a2b400238062c78c111abfc4ec0bd
[pr46]: https://github.com/robur-coop/miou/commit/ed46445a83c056e1152b826d08579edd3ec7c26e
[pr48]: https://github.com/robur-coop/miou/commit/2e82a64c3ecfd124525bf0ed93e00f215ce7a194
[pr47]: https://github.com/robur-coop/miou/commit/01b75212e595d154a0f24387e0fdde8cf86254f2
[pr51]: https://github.com/robur-coop/miou/commit/089eec9aedbd4feaf5bc1bce53585a0ec0bc810d
[pr52]: https://github.com/robur-coop/miou/commit/a8a7f395fc6cdfce1e071dd89a734e448f92e358
[pr56]: https://github.com/robur-coop/miou/commit/93296ad4289cffaf96d712f309d4a6b946d4cc89
[pr53]: https://github.com/robur-coop/miou/commit/c99705689cbf9845e92d9e2deda1586badd68871
[pr54]: https://github.com/robur-coop/miou/commit/a240782716843fdd5542ca479ca6720fcfc70752

### v0.3.0 (2024-08-22)

- Set the internal pipe used to interrupt a domain to a non-blocking mode and
  remove the usage of an atomic which protect how we fill the pipe
  (@haesbaert, @dinosaure, [#28][pr28])
- Expose option to reuse addr/port when we `Miou_unix.bind_and_listen`
  (@ada2k, @dinosaure, [#27][pr27])
- Protect an illegal access to the orphan from a possibly parallel task which
  does not own the orphan value
  (@polytypic, @dinosaure, [#31][is31], [#32][pr32])
- Be able to pin a specific domain when we want to launch a parallel task
  (@dinosaure, [#34][pr34])
- Expose the `Miou.Backoff` module which can be useful for users
  (@dinosaure, [#35][pr35])
- Fix or improve (from the maintenance point-of-view) the `Miou.Queue` module
  and some internal parts of Miou about the usage of atomics
  (@dinosaure, @polytypic, [#36][pr36], [#33][pr33])
- Prefer to require a `finaliser` function for the `events` value and actually
  close the internal `Unix.pipe` used to interrupt domain than to use
  `Gc.finaliser` and possibly leak file-descriptors
  (spotted by @hannesm, @dinosaure, [#37][pr37])

[pr28]: https://github.com/robur-coop/miou/commit/c148cd074727188730fdd8d01eb7049a15047a82
[pr27]: https://github.com/robur-coop/miou/commit/072de4fb6588d4bc2930d0fdd2e1672101fb58cf
[is31]: https://github.com/robur-coop/miou/issues/31
[pr32]: https://github.com/robur-coop/miou/commit/6959b444a0a59133e32a8cb5dff86021e8fab295
[pr34]: https://github.com/robur-coop/miou/commit/21e817fb814ee09fc29d9467a5307ab3302bdf8d
[pr35]: https://github.com/robur-coop/miou/commit/610d6c75fe905f9bd11025a49262581bf686d819
[pr36]: https://github.com/robur-coop/miou/commit/371c494e559a33e8103533d9d1046dae5a5df434
[pr33]: https://github.com/robur-coop/miou/commit/115f49d264260e8af52866c1653b7b39add3c965
[pr37]: https://github.com/robur-coop/miou/commit/b33361b3f607cf9616b84f59ae0ee14457ef8607

### v0.2.0 (2024-06-04)

- Don't try to abusively fill the pipe to interrupt a domain

  Interrupting a domain involves writing to a pipe to interrupt the `select(2)`
  if it is running. The pipe has a limited memory, depending on the system, and
  if you ask to interrupt a domain too much, you end up blocking the `write`.
  This patch prevents writing to the pipe if it has not yet been read.

  (@dinosaure, [#46][mr46])

- Expose the Sequence module
  (@dinosaure, [#47][mr47])
- Be able to add a hook (effect free) into the scheduler
  
  It is possible to add a hook to the scheduler. If the user wants to execute a
  function to a domain each time the domain is busy with a task, they can do so.
  However, the effects are not managed in the passed function.
  
  (@dinosaure, [#48][mr48])

- Add `Miou.Lazy`, a domain-safe `Lazy` module like `Stdlib.Lazy`
  (@dinosaure, initially implemented by @polytypic, [#49][mr49])
- Raise an exception if the user uses syscalls (from `Miou_unix`) and `Miou.run`
  instead of `Miou_unix.run`

  If a user uses a suspend function offered by `Miou_unix` but does not use
  `Miou_unix.run`, the programme may block indefinitely. This patch prevents
  such an error by raising an exception if we want to add a suspension point and
  we haven't specified how to handle it (if we use `Miou.run` instead of 
  `Miou_unix.run`).

  (@dinosaure, reported by @kit-ty-kate, [#51][mr51])

- Rename `Miou.set_signal` to `Miou.sys_signal`
  (@dinosaure, [#50][mr50])

- Improve `Miou_unix.{read,write}`
  (@kit-ty-kate, @dinosaure, [#52][mr52], 2f552a6, [#54][mr54])
- Fix an issue related to the dom0 and pending tasks locked by mutexes

  Tasks may have been transmitted to dom0 while it was executing a task and
  before the `select(2)`. This patch resynchronises the pending tasks in dom0's
  TODO-list before making the `select(2)`: specifically to find out whether the
  `select(2)` can block indefinitely or not. This patch also cleans up the old
  states of the tables used by `Miou_unix` if it is used on an ongoing basis (as
  in the case of tests).

  (@dinosaure, [#53][mr53])

- Add `Miou.Domain.available`
  (@dinosaure, [#53][mr53])
- Fix a race condition (observed with TSan) when we wait the cancellation of a
  children

  This patch changes Miou's behaviour a little when waiting for a task to be
  cancelled and prevents invalid access to a value that does not belong to the
  current domain (and which can be modified by another domain). Thanks
  @OlivierNicole and @fabbing for their advice on using TSan.

  (@dinosaure, [#56][mr56])

- Update the layout of Miou to avoid conflicts with other packages (like `backoff`)
  (@dinosaure, reported by @patricoferris, [#57][mr57])
- OCaml 5.3 support
  (@kit-ty-kate, [github#22][pr22])
- Rename `Miou.call_cc` to `Miou.async`
  (@dinosaure, @kit-ty-kate, @Armael, [github#23][pr23])

[mr46]: https://github.com/robur-coop/miou/commit/e647e48ceb90c5afdcc39110c3a786336cdcc824
[mr47]: https://github.com/robur-coop/miou/commit/78549a77ebd833bf83d609368637a204c5a68eef
[mr48]: https://github.com/robur-coop/miou/commit/82924daf3eea097081112c60f3f8f2235883707f
[mr49]: https://github.com/robur-coop/miou/commit/004fcc018d24b145772e8e6c768619e4dd2c21a8
[mr51]: https://github.com/robur-coop/miou/commit/72a1aab1c19109ad64abc105f577c5710e97f747
[mr50]: https://github.com/robur-coop/miou/commit/e269d54891dcf9697321e8263a46fd55cd87ad87
[mr52]: https://github.com/robur-coop/miou/commit/61378d8fd60ad6400e17cecd0fe852553027d1c9
[mr54]: https://github.com/robur-coop/miou/commit/9e14c3996326c28c3611b06f3cd758477fba4a1b
[mr53]: https://github.com/robur-coop/miou/commit/8e3cd3649759c01ea37661cc544626efd167b998
[mr56]: https://github.com/robur-coop/miou/commit/ed5087b832797616df073bd8ec9baed2ec4e474c
[mr57]: https://github.com/robur-coop/miou/commit/eaa397e5d35d7ec00d2b212e50e34f132aa840b5
[pr22]: https://github.com/robur-coop/miou/pull/22
[pr23]: https://github.com/robur-coop/miou/pull/23

### v0.1.0 (2024-04-03)

A major change in Miou's internals to incorporate the excellent work of
[Vesa Karvonen][vesa] available in his [picos][picos] project. This change
integrates the [Trigger][trigger] module and the [Computation][computation]
module. It also uses the [Await][await] effect, which we hope will be
standardised.

This addition has enabled us to integrate the new [Condition][condition] and
[Mutex][mutex] modules.

This change has enabled us to correctly formalise the resources used by our
scheduler and to release them accordingly, particularly with regard to the
cancellation whose behaviour is better defined.

We also took the opportunity to integrate the priority queue extracted from the
[Vocal][vocal] project and checked using [Why3][why3]. We would like to thank
their authors and maintainers for their help.

Finally, the API has changed very little and only ownership is no longer
mandatory when using the [Miou_unix][Miou_unix] module but is still available
through the [Miou_unix.Ownership][Miou_unix_Ownership] module.

As such, we have written [a tutorial][tutorial] that explains in detail what
Miou can offer and how to create applications with it. It's also a good
introduction to using effects and implementing a mini echo server and mini
scheduler.

The changes are far too profound to establish an accurate Changelog since the
last beta. However, we have tried to respect our previous tests as much as
possible and ensure continuity in what Miou has to offer despite its beta
status. We hope that, given the changes described above, users will understand
this **breaking-change**.

[vesa]: https://github.com/polytypic
[picos]: https://github.com/ocaml-multicore/picos
[trigger]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/sync.mli#L16
[computation]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/sync.mli#L70
[await]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/sync.mli#L44-L46
[condition]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/miou.mli#L1102
[mutex]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/miou.mli#L1068
[vocal]: https://github.com/ocaml-gospel/vocal
[why3]: https://www.why3.org/
[Miou_unix]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/miou_unix.mli#L1
[Miou_unix_Ownership]: https://github.com/robur-coop/miou/blob/cd4d8000204750d3c7e49512a63cddf725a079d1/lib/miou_unix.mli#L52
[tutorial]: https://robur-coop.github.io/miou/

### v0.0.1~beta2 (2024-01-04)

- Composition between Miou and user-defined effects. @dinosaure
  [664e8e26ec861c4fe3672144eead70b42f5fdbe5](https://github.com/robur-coop/miou/commit/664e8e26ec861c4fe3672144eead70b42f5fdbe5)

  Miou includes a mechanism that allows you to create an effects manager using
  Miou's effects manager. The latter always respects the rule: one **effect
  yields**. The documentation has been updated accordingly. Tests 28 and 31 show
  an example of effect composition.

- Integrate the test t26 & t27. @dinosaure
  [885d86eeedede3b28f6467bc2f0343b2ccf71225](https://github.com/robur-coop/miou/commit/885d86eeedede3b28f6467bc2f0343b2ccf71225)

  Test 26 verifies that Miou does indeed perform an infinite loop if a system
  event handler has not been installed and `Miou.suspend` is used. Test 27 is
  used to check Miou's behaviour with `Miou.yield`.

- Fix the happy-eyeballs example & cancelled suspension points. @dinosaure
  [ffa2fc059d62bf580ed6d090aaf33e26fe9571e8](https://github.com/robur-coop/miou/commit/ffa2fc059d62bf580ed6d090aaf33e26fe9571e8)
  [bf47c1a83ad5b8607dd097b6a1547cc18c0527c8](https://github.com/robur-coop/miou/commit/bf47c1a83ad5b8607dd097b6a1547cc18c0527c8)

  When a task is cancelled, we have to clean up the suspension points created
  in this task. The code has been factorised to avoid duplication between
  happy-eyeballs and dns.

- **breaking change** Be able to know if an `orphans` still have few tasks or
  not. The `Miou.care` returns an `'a t option option` where:
  + `Some (Some _)` is a ready-to-await task
  + `Some None` informs that some tasks still exists but they are not finished
  + `None` informs that the orphans has no more tasks

  @dinosaure
  [0560a14ea848466ea99792d27138c47f71aab747](https://github.com/robur-coop/miou/commit/bf47c1a83ad5b8607dd097b6a1547cc18c0527c8)

- Protect the `dom0` from user's exceptions. @dinosaure
  [0dd647e15a859d458d6daf3ca96e3e65a30ce30f](https://github.com/robur-coop/miou/commit/0dd647e15a859d458d6daf3ca96e3e65a30ce30f)

- **breaking change** The `give` argument no longer transfers ownership to the
  new task, but copies ownership so that the task that created the new task
  **and** the new task have ownership of the resource. @dinosaure
  [55ff54ddfd585f52fcda4d56caf2458760a7f949](https://github.com/robur-coop/miou/commit/55ff54ddfd585f52fcda4d56caf2458760a7f949)

- Add `Miou_unix.shutdown` (like `Unix.shutdown`). @dinosaure
  [7f37ed6645965552a34070b23177dfdc4dda01fd](https://github.com/robur-coop/miou/commit/7f37ed6645965552a34070b23177dfdc4dda01fd)

- Clean shutdown of the domain pool if one of the domains raises an exception.
  @dinosaure
  [27f3d1d09a170212ab291daad214da3511d972a4](https://github.com/robur-coop/miou/commit/27f3d1d09a170212ab291daad214da3511d972a4)

- Add a `self` function: This function lets you know which promise you're in. It
  provides information such as the domain executing the task, the promise
  identifier and the number of resources it is responsible for. @dinosaure
  [0b1833f623e61b674105cdced64aa624f2a6340e](https://github.com/robur-coop/miou/commit/0b1833f623e61b674105cdced64aa624f2a6340e)

- Expose _partially_ our `Logs` module. @dinosaure
  [e4062178f4dee18a97fbc3ebb6fb36352d9ff9f5](https://github.com/robur-coop/miou/commit/e4062178f4dee18a97fbc3ebb6fb36352d9ff9f5)

- Be able to handle multiple suspension points for the same file-descriptor.
  @dinosaure
  [1728d2600d0177f9422474bb41a1f12a111e6658](https://github.com/robur-coop/miou/commit/1728d2600d0177f9422474bb41a1f12a111e6658)

- Use `Heapq` instead of an `Hashtbl` to handle _sleepers_. @dinosaure
  [32e0b8adc7a6a92e14dae12d1fa1991ce41f71a5](https://github.com/robur-coop/miou/commit/32e0b8adc7a6a92e14dae12d1fa1991ce41f71a5)

- Avoid CSE optimisation via `Sys.opaque_identity`. @polytypic @dinosaure
  [5bcdaf4dd4fbf404cfb6f89b06280e12298c50cc](https://github.com/robur-coop/miou/commit/5bcdaf4dd4fbf404cfb6f89b06280e12298c50cc)

- Reverse control of task waits. Previously, Miou systematically added a step if
  we wanted to wait for the result of a task. If the task was completed, we
  ended up giving the result after several observations of the promise, which
  could be slow in certain situations.

  Now, waiting for a task attaches the continuation to the promise. If the task
  associated with the promise finishes, we simply "continue" with the value
  obtained avoiding a systematic observation of the state of the promise until
  the task finishes.

  @dinosaure
  [d795f08fc64f3e53077172dbfedfcefe47a2b832](https://github.com/robur-coop/miou/commit/d795f08fc64f3e53077172dbfedfcefe47a2b832)
  [42ef35bafa21ea0c264f6f3519fcb72732c1abd8](https://github.com/robur-coop/miou/commit/42ef35bafa21ea0c264f6f3519fcb72732c1abd8)

- Expose [Miou_unix.on_{read,write}]

  @dinosaure
  [27174019f959414051bfbdb5057a17e6b7a036ee](https://github.com/robur-coop/miou/commit/27174019f959414051bfbdb5057a17e6b7a036ee)

- Fix the documentation about [Miou.call] and the possible situation where no
  domain are available to execute a parallel task

  @dinosaure, @zapashcanon
  [aab9996ab2a3a59db2fed59f8ed997ef6f5abe14](https://github.com/robur-coop/miou/commit/aab9996ab2a3a59db2fed59f8ed997ef6f5abe14)

### v0.0.1~beta1 (2023-09-05)

- First release of `miou`
