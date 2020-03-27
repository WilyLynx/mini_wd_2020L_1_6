Scrapped from https://projekty.ncn.gov.pl/index.php?s=1 for s=1..17922.

For each page the following information has been scrapped:

* title
* id
* keywords
* descriptors
* panel
* panelDescription
* institution
* voivodeship
* manager
* coinvestigators
* contest
* announced
* budget
* start
* duration
* status

### grants.csv

* separator: * (asterisk)
* list item separator: **|||** (three pipes)
* **WARNING** problems with reading to R
* **BUG** when coinvestigators info was not present -- HTML tag is present instread of empty string

### grants_fixed.csv

* separator: **#**
* list item separator: **>**
* fixed missing coinvestigators

### grants_cleared.csv

* separator: **,** (comma)
* list item separator: **|**
* reduced information:
    * descriptors (only descriptor id, without description)
	* panel
	* coinvestigators
	* contest (without info about contest edition)
	* announced
	* budget (clear number)
	* start
	* duration (clear number)

### grants_mapped.csv

* separator: **,** (comma)
* list item separator: **|**
* information as for *grants_cleared.csv*
* old descriptors mapped to current ones (starting with zeros)
	
