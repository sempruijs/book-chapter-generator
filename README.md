# book notes generator

When you want to write notes for book chapters in Obsidian, it can be painful to create a lot of files.
Let say you want to write you thoughts on the book of Psalms in the bible.
You do not want to create 150 files of each psalm manualy.

This haskell program automates that.

### How to use

1. Create a file representing the last chapter in the book.

```bash
touch 'psalm 150.md'
```

Then enter the following contents:

```markdown
tags: [[bible]]
created: 2025-02-01

# Psalm 150

### References
- [[Psalms]]
```

Note that you can customise this however you want. It **should contain a heading ending with number**.

2. nix run 😎

```shell
nix run github:sempruijs/book-notes-generator
```

Enter the file name a whola! You have 150 files with correct numbers in the headings.
Enjoy and happy reading

### License

MIT license
