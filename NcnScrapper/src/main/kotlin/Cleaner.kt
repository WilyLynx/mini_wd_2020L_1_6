fun main() {
    val sourcePath = "..\\data\\grants_fixed.csv"
    val resultPath = "..\\data\\grants_cleared.csv"

    val data = NcnData.readFromFile(sourcePath, Separator)
    val resultData = data.map { clear(it) }
    ClearedNcnData.writeToFile(resultPath, ClearSeparator, resultData)
}

fun clear(record: NcnData): ClearedNcnData {

    val descriptors = record.descriptors.split(ListItemSeparator).joinToString(ClearListItemSeparator) { it.split(": ")[0] }
    val budget = record.budget.split("PLN")[0].replace(" ", "").toInt().toString()
    val duration = record.duration.split(" ")[0]
    val contest = record.contest.replace(Regex("[0-9]+"), "").trim()

    return ClearedNcnData(
        panel = record.panel,
        descriptors = descriptors,
        coinvestigators = record.coinvestigators,
        contest = contest,
        announced = record.announced,
        budget = budget,
        start = record.start,
        duration = duration
    )
}