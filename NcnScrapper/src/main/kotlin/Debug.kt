import org.jsoup.nodes.Element
import org.jsoup.select.Elements

fun print(title: String, elements: Elements) {
    val values = elements.map { it.html() }.reduce { acc, s -> "$acc$ListItemSeparator $s" }
    println("$title: $values")
}


fun print(title: String, element: Element) {
    println("$title: ${element.html()}")
}

fun print(title: String, value: String) {
    println("$title: $value")
}

fun Elements.printIterated() {
    this.forEachIndexed { index, element -> print(index.toString(), element) }
}