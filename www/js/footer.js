$(function(){
    function footerPosition(){
      $("footer").removeClass("fixed-footer");
      var contentHeight = document.body.clientHeight,
          winHeight = window.innerHeight;
      if((contentHeight < winHeight)){
          $("footer").addClass("fixed-footer");
          $(".content").height(winHeight);
      } else {
          $("footer").removeClass("fixed-footer");
          }
      }
      footerPosition();
      $(window).resize(footerPosition);
});
