# CLARITY 3.0.0 [uploaded 2026-02-12]

* Sidebar inputs are now dynamically enabled/disabled based on the rendering status of plots in each navbar tab for "Breed Analysis" and "Breed Comparison".
    This prevents users from changing inputs during long rendering times, which avoids excessive input requests and thus reduces waiting times.
    Changes are also relevant for the online version of CLARITY, improving performance and preventing crashes during use.

* Added a new "Run selected approaches" button for "Breed analysis", visible only when the user changes the approach selection (multiple selections are possible). 
    This prevents accidental triggering before the selection is ready. 
    * If the user changes the approach selection and want to see the outcome, the user
      must click the button "Run selected approaches" to execute the analysis. The button is hidden again. 
    * If the user changes the input of breed or chromosome, the analysis automatically starts due to the button being auto-clicked via `shinyjs::click()`, and the button is hidden. 
    * When the button is clicked then Breed analysis sidebar inputs are disabled until the corresponding plot is fully rendered.
    <br><br>
* Added a new "Run selected breeds" button for "Breed comparison", visible only when the user changes the approach selection (multiple selections are possible). This prevents accidental triggering before the selection is ready. 
    * If the user changes the breed selection and want to see the outcome, the user
      must click the button "Run selected breeds" to execute the analysis. The button is hidden again. 
    * If the user changes the input of chromosome or approach, the analysis automatically starts due to the button being auto-clicked via `shinyjs::click()`, and the button is hidden.     * When the button is clicked then Breed comparison sidebar inputs are disabled until the corresponding plot is fully rendered.
       <br> <br>
* Module IDs for Breed analysis and Breed comparison have been changed to dynamic IDs where needed, to prevent repeated execution of reactive elements in the modules.
* For modules with dynamic module IDs, it was necessary to add a `shinyjs::delay()` to ensure that `shinyjs elements` (e.g., `show`, `hide`)
    work correctly (server send signal before DOM is ready).
* Module state is now tracked to distinguish between a new module ID (using default settings) and reused module ID (settings retained). This is relevant for "Breed analysis" and "Breed comparison" when switching between sidebar elements (e.g., "Breed analysis", "Breed comparison") or corresponding navbar elements, and returning back to a previously active navbar tab (e.g., Hotspot detection for Breed analysis) to re-run the corresponding module with the same settings. This was implemented due to Plotly plots need to be re-rendered to ensure that they are correct displayed, which resolves the issue that Plotly plots were sometimes incomplete displayed.

* Added the "Plot hotspot only" option by Hotspot detection (available in both "Breed Analysis" and "Breed Comparison" tabs) to display only hotspot regions, improving visualization speed — especially when "All" chromosomes are selected.
* The "Threshold to define hotspot region" slider is also now enabled/disabled based on plot rendering status.

* Added the "choice" parameter to "transformdata_hotspot()" and "transformdata_hotspot_bc()" to control data filtering depending on the 
  "Plot hotspot only" option.
* Added the "max.plot" parameter to "scatterPlot_hotspot()" and "scatterPlot_hotspot_all()" to ensure the x-axis always spans the full chromosome length. In `scatterPlot_hotspot()`, the y-axis is also adjusted in view to maximum theta.

* Fixed minor bugs


# CLARITY 3.0.0 - pre-release 2 [uploaded on December 2025]

* added some relevant information
* some bugs have been fixed and some minor changes have been made

# CLARITY 3.0.0 - pre-release 1 [uploaded on September 2025]

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
