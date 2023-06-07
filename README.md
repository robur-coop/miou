# Miou, a simple scheduler for OCaml 5

Work-In-Progress
- [ ] a basic skeleton with effects
- [ ] be able to build `miou` without dependencies (be dependencies free for the code)
- [ ] implement some basic tests
- [ ] documentation
- [ ] explanation
- [ ] *every created promise must be either awaited or cancelled*
- [ ] *awaiting for a promise only returns after said promise has awaited its own children*
- [ ] *children can only be awaited by their direct parent. This excludes grandpas, grandmas, cousins, brothers and sisters*
- [ ] Add a test about our lock-free queue (like two domains which fill the queue and see the result)
- [ ] Implement `sleep`
