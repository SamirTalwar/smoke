# Release instructions for maintainers

So it's time to release a new version.

Versions must follow [Semantic Versioning 2.0.0][].

1. Bump the version in _package.yaml_.
2. Correct the version accordingly in _spec/version.yaml_.
3. Run `make check`.
4. Commit the changes and push to the `main` branch.
5. Tag `HEAD` with the version (`git tag vX.Y.Z HEAD`).
6. Push the new tag with `git push --tags`.
7. Wait until the draft release shows up, with all artifacts.
8. Add a description and publish it.

[Semantic Versioning 2.0.0]: https://semver.org/spec/v2.0.0.html
