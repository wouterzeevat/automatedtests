# NEWS

## Version 0.1.0 (2025-05-01)

- Initial release.

# Version 0.1.1 (2025-05-19)

- Added dissapeared tests. (Qualitative x quantitative tests for 2 groups)
- Fixed a bug where test did not run properly.
- Changed package 'RVAideMemoire' -> 'DescTools' to match new versions.
- Added test strength, for more clear and insightful results.
- Added colors to a printed tests for a more clear result.

# Version 0.1.2 (2025-06-16)

- Added better documents
- Added warnings.
- Created table output for multiple variable tests.
- Parametric_list() outputs a dataframe instead of a list.
- Object's functions are changed to snakecase to match R standard.

# Version 0.1.3 (2026-09-09)

- Fixed test selection checking normality on the pooled data instead of within each group.
  Two normal groups with different means were wrongly sent to a non-parametric test.
- Paired designs now check normality of the paired differences.
- get_parametric_list() reports one row per tested sample, e.g. "value (group = A)".
- Samples too small to test for normality no longer error and are reported as NA.
- Added a testthat test suite.
- Pinned CI runners to ubuntu-24.04.
