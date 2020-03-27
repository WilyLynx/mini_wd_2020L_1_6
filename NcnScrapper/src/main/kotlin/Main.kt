import proc.Cleaner
import proc.DescriptorMapper
import scrap.Fixer
import scrap.Scrapper

fun main() {
    map()
}

fun scrap() = Scrapper().run()

fun fix() = Fixer().run()

fun clean() = Cleaner().run()

fun map() = DescriptorMapper().run()