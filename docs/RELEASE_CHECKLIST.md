# Release Checklist

This document contains list of actions needed to be done before releasing a new
version of Iris.

- [ ] Go through commit history and update `CHANGELOG.md`
- [ ] Bump up version in `iris.cabal`
- [ ] Update versions of all newly introduced types and functions using the
      following command

    ```shell
    # Linux
    rg "x\.x\.x\.x" --files-with-matches | xargs sed -i 's/x.x.x.x/1.3.2.0/g'

    # macOS
    rg "x\.x\.x\.x" --files-with-matches | xargs sed -i '' 's/x.x.x.x/1.3.2.0/g'
    ```

- [ ] Updated `expectedNumericVersion` `Test.Iris.Cli` to the new one
- [ ] Create a new release on GitHub with the new version
- [ ] Upload dist to Hackage
- [ ] (Optional) Upload documentation to Hackage