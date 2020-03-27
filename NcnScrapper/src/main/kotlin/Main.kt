import proc.Cleaner
import proc.DescriptorMapper
import scrap.Fixer
import scrap.Scrapper

fun main() {
}

fun scrap() = Scrapper().run()

fun fix() = Fixer().run()

fun clean() = Cleaner().run()

fun map() = DescriptorMapper().run()

fun test() = Tester().run()