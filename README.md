# Article Model Generator

Article Model Generator for libcmark Markdown AST. You write your aricles or
blogs in markdown and `amg` can convert them into JSON that you can use with
frontend frameworks to render your article.

## Usage

```sh
amg my-markdown-file.md
```

## Install from Source

```sh
git clone git@github.com:marans/amg.git
cd amg ; stack install --local-bin-path /usr/local/bin/
```

### Short Example

`amg` can convert the following markdown

```md
# Blog title

Blog *content **is** short*.
```

to easy to use JSON..

```json
{
  "type": "document",
  "data": [
    {
      "type": "header",
      "level": 1,
      "data": [{ "markup": [], "content": "Blog title" }]
    },
    {
      "type": "paragraph",
      "data": [
        { "markup": [], "content": "Blog " },
        { "markup": ["emphasis"], "content": "content " },
        { "markup": ["emphasis", "strong"], "content": "is" },
        { "markup": ["emphasis"], "content": " short" },
        { "markup": [], "content": "." }
      ]
    }
  ]
}
```
