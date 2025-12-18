# CLARITY 3.0.0 [uploaded on September 2025]

* added some relevant information and prepared for new tag version
* fixed some bugs and smaller changes 

# CLARITY 3.0.0-pre [uploaded on September 2025]

* Results from the Hidden-Markow-Model (HMM) are included. The HMM approach allows to estimate sex-specific recombination rates (male, female), and also the sex-average. On this basis, significant changes as well as further optimization were made to the App. 
* Generally, improved differentiation of approaches within "Hotspot detection" (possible for the HMM-approach and deterministic approach) and "Genetic map  functions" (only available for the likelihood-based approach) has been implemented.
* "Breed analysis": approach comparison is now possible and also within the "Hotspot detection" the functional Venn diagram was added to enable hotspot comparison across approaches.
* "Breed comparison": approach selection was added
* Removed functions:
    * fct_general_hovering.R
    * scatterPlot_general_all_bc
    * scatterPlot_general_selected_bc 
    * get_only_legend 
    * makePlot_all_geneticMaps_bc
    * makePlot_geneticMaps_bc
* New functions:
    * create.output
    * create.colors
    * create_table_* (the custom created table headers were separated in individual functions to simplify the code)
* Added "Save genetic map data" for "Breed analysis" and "Breed comparison" to the "Genetic map" tab when all chromosomes are selected 
* Added description of used internal data and linked within the manual
* Adapt and modify corresponding documentations



# CLARITY 2.0.0 [uploaded on September 2024]

* Integrated Swiss cattle breeds (Angus, Braunvieh, Brown Swiss, Holstein-CH, Simmental, Limousin, Unifying-CH) and corresponding data sets information and misplaced markers were added
* Added a likelihood quality signal indicating the goodness of the likelihood-based approach
* Improved section "Breed comparison" and some smaller improvements
* Functions process_venn_data(), creating_venn() and prepare_table_venn() have been changed because the R package ggVennDiagram has been updated (geom_sf() must be replaced by geom_polygon)
* Typos and fixed minor bug
* Added “styles.css” to remove most of the CSS code from app_ui.R
* Added "custom.js" to change attributes of elements where an ARIA conflict was observed 
* Added data license

# CLARITY 1.0.1 [uploaded on May 2023]

* Typos, fixed minor bug, and license definition

# CLARITY 1.0.0 [uploaded on October 2022]

* First comprehensive version including Holstein and Fleckvieh breed.

# CLARITY 0.0.1.0000 [uploaded on June 2022]

* Smaller changes were realized. 

# CLARITY 0.0.0.9000 [uploaded on April 2022]

* Initial upload. 
