  $( document ).ready(function() {

$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_click',this.id);
  document.getElementById('naranja_body').style.backgroundImage="url(Background-fill@2x.png)"

 });

$(document).on('click', '.needed', function(){
    $('.needed').removeClass('basic_active');
    $(this).addClass('basic_active');
});


});
