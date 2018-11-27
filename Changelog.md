### 0.5.0: November 27, 2018

Features:
* New global option `EjudgeServeConfigurationsPath`
* New local option `DisplayedColumns`
* New local option `RowSortingOrder`
* New local section `ContestNamePattern`
* Implemented Ejudge configuration parsing for some unsupported before options
  like penalties for unsuccessful submits
* Command-line interface for changing some global options
* Full localization for Russian and English language
* Automatic language detection by HTTP headers

Changes:
* `SetDeadlinePenalty` renamed to just `DeadlinePenalty`
* Not started contests are now skipped from rendering instead of throwing an
  exception on finding one
* A lot of internal refactoring done

Fixed:
* Fixed a bug with leading zeroes in fractions
* Fixed crashing on too many file descriptors

### 0.4.0: September 19, 2018

* XML parser had been rewritten from xml-conduit to xeno library,
  and is almost 50% faster

### 0.3.1: September 10, 2018

* Third-party licenses are now included in project
* New legal credits page

### 0.3.0: September 9, 2018

* New global option `WebRoot`
* New local option `ShowProblemStatistics`
* New local option `ReversedContestOrder`
* New local section `ConditionalStyle`
* Fixed slow reading of run statuses
* Local configurations are reloaded on every request
