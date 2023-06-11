# Miou, a simple scheduler for OCaml 5

Work-In-Progress
- [x] a basic skeleton with effects
- [ ] be able to build `miou` without dependencies (be dependencies free for the code)
- [ ] implement some basic tests
- [ ] documentation
- [ ] explanation
- [ ] *every created promise must be either awaited or cancelled*
- [ ] *awaiting for a promise only returns after said promise has awaited its own children*
- [x] *children can only be awaited by their direct parent. This excludes grandpas, grandmas, cousins, brothers and sisters*
      a test was made, see t07
- [ ] add a test about our lock-free queue (like two domains which fill the queue and see the result)
- [x] implement `sleep`
- [ ] implement `yield` à la `affect`
- [x] properly `Domain.join` domains at the end
