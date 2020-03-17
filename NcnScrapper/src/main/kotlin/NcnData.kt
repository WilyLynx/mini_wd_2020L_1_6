import kotlin.reflect.full.memberProperties

data class NcnData(
    val title: String,
    val id: String,
    val keywords: String,
    val descriptors: String,
    val panel: String,
    val panelDescription: String,
    val institution: String,
    val voivodeship: String,
    val manager: String,
    val coinvestigators: String,
    val contest: String,
    val announced: String,
    val budget: String,
    val start: String,
    val duration: String,
    val status: String
) {
    override fun toString(): String {
        return NcnData::class.memberProperties.joinToString("\n") { "${it.name}: ${it.get(this)}" }
    }

    fun toCsvRecord(): String {
        return NcnData::class.memberProperties.joinToString(",") { "\"${it.get(this).toString().trim()}\"" }
    }
}

fun ncnDataHeaders() = NcnData::class.memberProperties.joinToString(",") { "\"${it.name}\"" }