### v0.0.1~beta2 (2024-01-04)

- Composition between Miou and user-defined effects. @dinosaure
  [664e8e26ec861c4fe3672144eead70b42f5fdbe5](https://git.robur.coop/robur/miou/commit/664e8e26ec861c4fe3672144eead70b42f5fdbe5)

  Miou includes a mechanism that allows you to create an effects manager using
  Miou's effects manager. The latter always respects the rule: one **effect
  yields**. The documentation has been updated accordingly. Tests 28 and 31 show
  an example of effect composition.

- Integrate the test t26 & t27. @dinosaure
  [885d86eeedede3b28f6467bc2f0343b2ccf71225](https://git.robur.coop/robur/miou/commit/885d86eeedede3b28f6467bc2f0343b2ccf71225)

  Test 26 verifies that Miou does indeed perform an infinite loop if a system
  event handler has not been installed and `Miou.suspend` is used. Test 27 is
  used to check Miou's behaviour with `Miou.yield`.

- Fix the happy-eyeballs example & cancelled suspension points. @dinosaure
  [ffa2fc059d62bf580ed6d090aaf33e26fe9571e8](https://git.robur.coop/robur/miou/commit/ffa2fc059d62bf580ed6d090aaf33e26fe9571e8)
  [bf47c1a83ad5b8607dd097b6a1547cc18c0527c8](https://git.robur.coop/robur/miou/commit/bf47c1a83ad5b8607dd097b6a1547cc18c0527c8)

  When a task is cancelled, we have to clean up the suspension points created
  in this task. The code has been factorised to avoid duplication between
  happy-eyeballs and dns.

- **breaking change** Be able to know if an `orphans` still have few tasks or
  not. The `Miou.care` returns an `'a t option option` where:
  + `Some (Some _)` is a ready-to-await task
  + `Some None` informs that some tasks still exists but they are not finished
  + `None` informs that the orphans has no more tasks

  @dinosaure
  [0560a14ea848466ea99792d27138c47f71aab747](https://git.robur.coop/robur/miou/commit/bf47c1a83ad5b8607dd097b6a1547cc18c0527c8)

- Protect the `dom0` from user's exceptions. @dinosaure
  [0dd647e15a859d458d6daf3ca96e3e65a30ce30f](https://git.robur.coop/robur/miou/commit/0dd647e15a859d458d6daf3ca96e3e65a30ce30f)

- **breaking change** The `give` argument no longer transfers ownership to the
  new task, but copies ownership so that the task that created the new task
  **and** the new task have ownership of the resource. @dinosaure
  [55ff54ddfd585f52fcda4d56caf2458760a7f949](https://git.robur.coop/robur/miou/commit/55ff54ddfd585f52fcda4d56caf2458760a7f949)

- Add `Miou_unix.shutdown` (like `Unix.shutdown`). @dinosaure
  [7f37ed6645965552a34070b23177dfdc4dda01fd](https://git.robur.coop/robur/miou/commit/7f37ed6645965552a34070b23177dfdc4dda01fd)

- Clean shutdown of the domain pool if one of the domains raises an exception.
  @dinosaure
  [27f3d1d09a170212ab291daad214da3511d972a4](https://git.robur.coop/robur/miou/commit/27f3d1d09a170212ab291daad214da3511d972a4)

- Add a `self` function: This function lets you know which promise you're in. It
  provides information such as the domain executing the task, the promise
  identifier and the number of resources it is responsible for. @dinosaure
  [0b1833f623e61b674105cdced64aa624f2a6340e](https://git.robur.coop/robur/miou/commit/0b1833f623e61b674105cdced64aa624f2a6340e)

- Expose _partially_ our `Logs` module. @dinosaure
  [e4062178f4dee18a97fbc3ebb6fb36352d9ff9f5](https://git.robur.coop/robur/miou/commit/e4062178f4dee18a97fbc3ebb6fb36352d9ff9f5)

- Be able to handle multiple suspension points for the same file-descriptor.
  @dinosaure
  [1728d2600d0177f9422474bb41a1f12a111e6658](https://git.robur.coop/robur/miou/commit/1728d2600d0177f9422474bb41a1f12a111e6658)

- Use `Heapq` instead of an `Hashtbl` to handle _sleepers_. @dinosaure
  [32e0b8adc7a6a92e14dae12d1fa1991ce41f71a5](https://git.robur.coop/robur/miou/commit/32e0b8adc7a6a92e14dae12d1fa1991ce41f71a5)

- Avoid CSE optimisation via `Sys.opaque_identity`. @polytypic @dinosaure
  [5bcdaf4dd4fbf404cfb6f89b06280e12298c50cc](https://git.robur.coop/robur/miou/commit/5bcdaf4dd4fbf404cfb6f89b06280e12298c50cc)

- Reverse control of task waits. Previously, Miou systematically added a step if
  we wanted to wait for the result of a task. If the task was completed, we
  ended up giving the result after several observations of the promise, which
  could be slow in certain situations.

  Now, waiting for a task attaches the continuation to the promise. If the task
  associated with the promise finishes, we simply "continue" with the value
  obtained avoiding a systematic observation of the state of the promise until
  the task finishes.

  @dinosaure
  [d795f08fc64f3e53077172dbfedfcefe47a2b832](https://git.robur.coop/robur/miou/commit/d795f08fc64f3e53077172dbfedfcefe47a2b832)
  [42ef35bafa21ea0c264f6f3519fcb72732c1abd8](https://git.robur.coop/robur/miou/commit/42ef35bafa21ea0c264f6f3519fcb72732c1abd8)

### v0.0.1~beta1 (2023-09-05)

- First release of `miou`
