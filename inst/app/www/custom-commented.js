document.addEventListener("DOMContentLoaded", function() {

    // for header - navbar - fa-bars icon
   var toggleButtons = document.querySelectorAll('[role=\"navigation\"]');
      toggleButtons.forEach(function(button) {
        var icon = button.querySelector('i');
        if (icon) {
          icon.setAttribute('aria-label', 'navigation');
          icon.setAttribute('role', 'button');
        }
      });


  //  for '>>' the icon' labels  sidebar -- set correct labels
  var togButtons = document.querySelectorAll('[data-toggle=\"tab\"]');
      togButtons.forEach(function(button) {
        var icon = button.querySelector('i');
        if (icon) {
          icon.setAttribute('aria-label', 'tab');
          icon.setAttribute('role', 'button');
        }
      });

  //adding the aria - element title to selectInput - has to be done for each selection button itself
  document.getElementById('breed').setAttribute('title', 'Breed');
  document.getElementById('breed1').setAttribute('title', 'Select breeds');
  document.getElementById('chromosome').setAttribute('title', 'chromosome');
  document.getElementById('chromosome1').setAttribute('title', 'chromosome');
  document.getElementById('mod_misplaced_1-breedSelect_misplaced').setAttribute('title','Select breed(s)');

  // plus/minus adaptation for aria for the shinydashboard::box!- 01.08.2024 - not working for the navbar-icon fa-bars
  var collapseButtons = document.querySelectorAll('[data-widget=\"collapse\"]');
          collapseButtons.forEach(function(button) {
            var icon = button.querySelector('i');
            if (icon) {
              icon.setAttribute('aria-label', 'collapse');
              icon.setAttribute('role', 'button');
            }
          });

  // change the aria role and aria-label for the icon by the download button
  var downloadButtons = document.querySelectorAll('.btn.btn-default.shiny-download-link');
        downloadButtons.forEach(function(button) {
          var icon = button.querySelector('i');
          if (icon) {
            icon.setAttribute('aria-label', 'Download data');
            icon.setAttribute('role', 'button');
          }
        });

});



