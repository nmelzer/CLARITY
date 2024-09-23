document.addEventListener("DOMContentLoaded", function() {
   var toggleButtons = document.querySelectorAll('[role=\"navigation\"]');
      toggleButtons.forEach(function(button) {
        var icon = button.querySelector('i');
        if (icon) {
          icon.setAttribute('aria-label', 'navigation');
          icon.setAttribute('role', 'button');
        }
      });
  var togButtons = document.querySelectorAll('[data-toggle=\"tab\"]');
      togButtons.forEach(function(button) {
        var icon = button.querySelector('i');
        if (icon) {
          icon.setAttribute('aria-label', 'tab');
          icon.setAttribute('role', 'button');
        }
      });
  document.getElementById('breed').setAttribute('title', 'Breed');
  document.getElementById('breed1').setAttribute('title', 'Select breeds');
  document.getElementById('chromosome').setAttribute('title', 'chromosome');
  document.getElementById('chromosome1').setAttribute('title', 'chromosome');
  document.getElementById('mod_misplaced_1-breedSelect_misplaced').setAttribute('title','Select breed(s)');
  var collapseButtons = document.querySelectorAll('[data-widget=\"collapse\"]');
          collapseButtons.forEach(function(button) {
            var icon = button.querySelector('i');
            if (icon) {
              icon.setAttribute('aria-label', 'collapse');
              icon.setAttribute('role', 'button');
            }
          });
  var downloadButtons = document.querySelectorAll('.btn.btn-default.shiny-download-link');
        downloadButtons.forEach(function(button) {
          var icon = button.querySelector('i');
          if (icon) {
            icon.setAttribute('aria-label', 'Download data');
            icon.setAttribute('role', 'button');
          }
        });
});
