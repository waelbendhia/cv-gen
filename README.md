# cv-gen
Program that generates CVs from yaml files.
The class file is adapted from [mcdowell-cv](https://github.com/dnl-blkv/mcdowell-cv).

# Building
Just run `cabal build`.

# Usage
You've got three options
- `about` or `a`: use `short` for short summary, `long` for long summary, `none` to omit summary
- `projects` or `p`: use `short` to list projects without technologies, `long` to list with technologies, `none` to omit projects entirely
- `priority`: minimum priority level to include project

See [example.yaml](example/example.yaml) and different ouputs at [example-all-long.pdf](example/example-all-long.pdf), [example-all-short.pdf](example/example-all-short.pdf) and [example-default.pdf](example/example-default.pdf).

# TODO
- [ ] Options to filter projects based on technologies/type
- [ ] More helpful readme
- [ ] More helpful option messages
- [ ] Options to select fonts, colors, etc...
- [x] Maybe some way to interactively select projects, options, etc...
