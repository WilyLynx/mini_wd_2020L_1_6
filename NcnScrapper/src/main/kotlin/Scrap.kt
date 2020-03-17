import org.jsoup.nodes.Element
import org.jsoup.select.Elements

const val ListItemSeparator = "|||"

fun Element.extractNcnData(): NcnData {
    val children = this.children()

    val title = children[0].html()
    val id = children[2].html()
    val keywords = children[6].select("a").joinToString()
    val descriptors = children[9].select("li").joinToString()
    val panel = children[10].select("a").first().extractBeforeSpan()
    val panelDescription = children[10].extractPanelDescription()
    val institution = children[13].select("p")[1].html()
    val voivodeship = children[13].select("p")[2].html()
    val manager = children[14].select("a").first().extractBeforeHardSpace()
    val coinvestigators = children[14].select("p").last().extractAfterColon()
    val contest = children[17].select("a").first().extractBeforeSpan()
    val announced = children[17].extractCompetitionAnnounced()
    val budget = children[18].select("p")[0].extractAfterColon()
    val start = children[18].select("p")[1].extractAfterColon()
    val duration = children[18].select("p")[2].extractAfterColon()
    val status = children[18].select("p")[3].extractAfterColon()

    val data = NcnData(
        title = title,
        id = id,
        keywords = keywords,
        descriptors = descriptors,
        panel = panel,
        panelDescription = panelDescription,
        institution = institution,
        voivodeship = voivodeship,
        manager = manager,
        coinvestigators = coinvestigators,
        contest = contest,
        announced = announced,
        budget = budget,
        start = start,
        duration = duration,
        status = status
    )
    return data
}

fun Elements.joinToString(): String {
    return this.map { it.html() }.reduce { acc, s -> "$acc$ListItemSeparator$s" }
}

fun Element.extractAfterColon(): String {
    return this.html().substringAfter(": ")
}

fun Element.extractBeforeHardSpace(): String {
    return this.html().substringBefore("&nbsp;")
}

fun Element.extractCompetitionAnnounced(): String {
    return this.html().substringAfter("ogłoszony").substringBefore("</p>")
}

fun Element.extractBeforeSpan(): String {
    return this.html().substringBefore("<span>")
}

fun Element.extractPanelDescription(): String {
    return this.html().substringAfter("</a> - ").substringBefore("</p>")
}