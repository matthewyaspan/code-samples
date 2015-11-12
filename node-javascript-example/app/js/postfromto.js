
//var stops = [];
var buttonClicks = 0;
var stuff;

      $("button.btn.btn-default").click(function(e) {
      var fT = [];
   if ($("#input").val() != "") {

        buttonClicks = buttonClicks + 1;
        if (buttonClicks == 1) {
 
          fT.push( $("input.form-control").val() );

          $("label.subtitle.space-right").fadeOut(500, function() {
            $("label.subtitle.space-right").text("where are you coming from?").fadeIn(500);
          });

        } else if (buttonClicks == 2) {

          fT.push( $("input.form-control").val() );
          from = fT[0];
          to = fT[1];
          $('body').load()
            $.ajax({
                type : 'POST',
                url : "/fromTo",
                data: {from, to},
                dataType : 'html',
                success : function(data, textStatus, jqXHR) {
                  console.log(data);
                  $('body').html(data);
                }
            });

      }

          $("input.form-control").val(''); 
    }
            // if valid input, render the map
            // potentialy some sort of animation to make the transition smooth
        /*} else if (buttonClicks == 3) {
           // 
            $("#label").val("Need to make a stop? ;)");
            stops.push({""})

        } else {

            $("#label").val("How 'bout another?");

        }

    } */

});

/*.$(document).keypress(function(e) {
 console.log("FORK");
 if (e.which == 13) 
   $("button.btn.btn-defau").trigger("click");
}); */
