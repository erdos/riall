# Riall - Simple Sankey diagramming

A simple tool to generate [Sankey diagrams](https://en.wikipedia.org/wiki/Sankey_diagram) from text input.

<table>
  <tr>
    <td><img width="240" src="https://github.com/erdos/riall/assets/5559823/29efb2d0-9ba9-43a9-8bcd-b75889edca8d"/></td>
    <td><img width="240" src="https://github.com/erdos/riall/assets/5559823/c7258059-ac99-4bec-a82d-24788fc1232c"/></td>
    <td><img width="240" src="https://github.com/erdos/riall/assets/5559823/81de01bc-3ff0-418f-adbf-4e21e7d4c5ec"/></td>
  </tr>
</table>

## Features

- Text in, SVG out
- Supports circular graphs, cycles are drawn with back edges
- Optimizes layout to minimize edge crossings
- Standalone application works offline

## Usage

### Standalone application

The program reads the graph definition from STDIN and writes the SVG output to STDOUT. Error messages and warnings are written to STDERR.

```bash
$ cat input.txt | riall > output.svg
```

### Input file format

The input file contains one line per edge. Each such line contains the source node, the weight of the edge (enclosed in square brackets) and the target node. The nodes can be any string that does not contain whitespace or square brackets.

```text
First Node [123] Second Node
Second Node [456] Finito
```

Any line starting with `//` or `#` is considered a comment and is ignored. The file can also contain empty lines. These are ignored as well.

The default style of elements can be customized by adding configuration lines as the following. For node properties:

```text
set node.background blue
set node.stroke.color white
set node.stroke.width 1
set node.width 8
set node.opacity 1.0
set node.shape.rx 3
set node.shape.ry 3
set node.label.align center
set node.label.background rgba(255,255,255,0.2)
```

For edge properties:

```text
set edge.background #ffadef
set edge.stroke.color #000000
set edge.stroke.width 1
set edge.opacity 0.7
```

Edge background can be `source`, `target`, `gradient`, or a color value.

For the whole canvas:

```text
set canvas.background #ffffff
set canvas.width 800
set canvas.height 600
set canvas.margin 10
```


### Other output formats

The program produces SVG output. This can be easily converted to other formats using the `convert` command from the [ImageMagick](https://imagemagick.org/) as such: `cat input.txt | riall | convert svg:- output.png`


### Contributions

Contributions are welcome. Please open an issue or a pull request.

## License

This project is released under the permissive MIT license. See [LICENSE](LICENSE) for details.
