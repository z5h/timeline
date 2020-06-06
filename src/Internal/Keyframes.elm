module Internal.Keyframes exposing (keyframes)


keyframes : String
keyframes =
    """/*=== scale_in ===*/
       .z5h_timeline__scale_in.start {
         opacity: 0;
         pointer-events: none;
         visibility: hidden;
         max-width: 0;
         max-height: 0;
         overflow: hidden;
       }
       .z5h_timeline__scale_in.in-progress {
         pointer-events: none;
         overflow: hidden;
       }
       .z5h_timeline__scale_in.end {
         max-width: 1920px;
         max-height: 1080px;
       }

       @keyframes z5h_timeline__scale_in__0 {
         from {
           opacity: 0;
           pointer-events: none;
           visibility: hidden;
           max-width: 0;
           max-height: 0;
         }
         to {
           opacity: 1;
           pointer-events: auto;
           visibility: visible;
           max-width: 1920px;
           max-height: 1080px;
         }
       }
       @keyframes z5h_timeline__scale_in__1 {
         from {
           opacity: 0;
           pointer-events: none;
           visibility: hidden;
           max-width: 0;
           max-height: 0;
         }
         to {
           opacity: 1;
           pointer-events: auto;
           visibility: visible;
           max-width: 1920px;
           max-height: 1080px;
         }
       }
       /*=== scale_in_x ===*/
       .z5h_timeline__scale_in_x.start {
         opacity: 0;
         pointer-events: none;
         visibility: hidden;
         max-width: 0;
         overflow: hidden;
       }
       .z5h_timeline__scale_in_x.in-progress {
         pointer-events: none;
         overflow: hidden;
       }
       .z5h_timeline__scale_in_x.end {
         max-width: 1920px;
       }

       @keyframes z5h_timeline__scale_in_x__0 {
         from {
           opacity: 0;
           pointer-events: none;
           visibility: hidden;
           max-width: 0;
         }
         to {
           opacity: 1;
           pointer-events: auto;
           visibility: visible;
           max-width: 1920px;
         }
       }
       @keyframes z5h_timeline__scale_in_x__1 {
         from {
           opacity: 0;
           pointer-events: none;
           visibility: hidden;
           max-width: 0;
         }
         to {
           opacity: 1;
           pointer-events: auto;
           visibility: visible;
           max-width: 1920px;
         }
       }
       /*=== scale_in_y ===*/
       .z5h_timeline__scale_in_y.start {
         opacity: 0;
         pointer-events: none;
         visibility: hidden;
         max-height: 0;
         overflow: hidden;
       }
       .z5h_timeline__scale_in_y.in-progress {
         pointer-events: none;
         overflow: hidden;
       }
       .z5h_timeline__scale_in_y.end {
         max-height: 1080px;
       }

       @keyframes z5h_timeline__scale_in_y__0 {
         from {
           opacity: 0;
           pointer-events: none;
           visibility: hidden;
           max-height: 0;
         }
         to {
           opacity: 1;
           pointer-events: auto;
           visibility: visible;
           max-height: 1080px;
         }
       }
       @keyframes z5h_timeline__scale_in_y__1 {
         from {
           opacity: 0;
           pointer-events: none;
           visibility: hidden;
           max-height: 0;
         }
         to {
           opacity: 1;
           pointer-events: auto;
           visibility: visible;
           max-height: 1080px;
         }
       }
       /*=== rotate_90 ===*/
       .z5h_timeline__rotate_90.end {
         transform: rotate(90deg);
       }

       @keyframes z5h_timeline__rotate_90__0 {
         from {
           transform: rotate(90deg);
         }
         to {
           transform: rotate(0);
         }
       }
       @keyframes z5h_timeline__rotate_90__1 {
         from {
           transform: rotate(90deg);
         }
         to {
           transform: rotate(0);
         }
       }
       /*=== rotate_180 ===*/
       .z5h_timeline__rotate_180.end {
         transform: rotate(180deg);
       }

       @keyframes z5h_timeline__rotate_180__0 {
         from {
           transform: rotate(180deg);
         }
         to {
           transform: rotate(0);
         }
       }
       @keyframes z5h_timeline__rotate_180__1 {
         from {
           transform: rotate(180deg);
         }
         to {
           transform: rotate(0);
         }
       }
       /*=== rotate_neg90 ===*/
       .z5h_timeline__rotate_neg90.end {
         transform: rotate(-90deg);
       }

       @keyframes z5h_timeline__rotate_neg90__0 {
         from {
           transform: rotate(-90deg);
         }
         to {
           transform: rotate(0);
         }
       }
       @keyframes z5h_timeline__rotate_neg90__1 {
         from {
           transform: rotate(-90deg);
         }
         to {
           transform: rotate(0);
         }
       }
       /*=== rotate_neg180 ===*/
       .z5h_timeline__rotate_neg180.end {
         transform: rotate(-180deg);
       }

       @keyframes z5h_timeline__rotate_neg180__0 {
         from {
           transform: rotate(-180deg);
         }
         to {
           transform: rotate(0);
         }
       }
       @keyframes z5h_timeline__rotate_neg180__1 {
         from {
           transform: rotate(-180deg);
         }
         to {
           transform: rotate(0);
         }
       }
       /*=== slide_in_left ===*/
       .z5h_timeline__slide_in_from_left.end {
         transform: translateX(-100%);
       }

       @keyframes z5h_timeline__slide_in_from_left__0 {
         from {
           transform: translateX(-100%);
         }
         to {
           transform: translateX(0);
         }
       }
       @keyframes z5h_timeline__slide_in_from_left__1 {
         from {
           transform: translateX(-100%);
         }
         to {
           transform: translateX(0);
         }
       }
       /*=== slide_in_right ===*/
       .z5h_timeline__slide_in_from_right.end {
         transform: translateX(100%);
       }

       @keyframes z5h_timeline__slide_in_from_right__0 {
         from {
           transform: translateX(100%);
         }
         to {
           transform: translateX(0);
         }
       }
       @keyframes z5h_timeline__slide_in_from_right__1 {
         from {
           transform: translateX(100%);
         }
         to {
           transform: translateX(0);
         }
       }
       /*=== slide_in_top ===*/
       .z5h_timeline__slide_in_from_top.end {
         transform: translateY(-100%);
       }

       @keyframes z5h_timeline__slide_in_from_top__0 {
         from {
           transform: translateY(-100%);
         }
         to {
           transform: translateY(0);
         }
       }
       @keyframes z5h_timeline__slide_in_from_top__1 {
         from {
           transform: translateY(-100%);
         }
         to {
           transform: translateY(0);
         }
       }
       /*=== slide_in_bottom ===*/
       .z5h_timeline__slide_in_from_bottom.end {
         transform: translateY(100%);
       }

       @keyframes z5h_timeline__slide_in_from_bottom__0 {
         from {
           transform: translateY(100%);
         }
         to {
           transform: translateY(0);
         }
       }
       @keyframes z5h_timeline__slide_in_from_bottom__1 {
         from {
           transform: translateY(100%);
         }
         to {
           transform: translateY(0);
         }
       }
       /*=== pulse ===*/
       @keyframes z5h_timeline__pulse__0 {
         from {
           transform: scale3d(1, 1, 1);
         }
         50% {
           transform: scale3d(1.05, 1.05, 1.05);
         }
         to {
           transform: scale3d(1, 1, 1);
         }
       }
       @keyframes z5h_timeline__pulse__1 {
         from {
           transform: scale3d(1, 1, 1);
         }
         50% {
           transform: scale3d(1.05, 1.05, 1.05);
         }
         to {
           transform: scale3d(1, 1, 1);
         }
       }
       /*=== flash ===*/
       @keyframes z5h_timeline__flash__0 {
         from, 50%, to {
           opacity: 1;
         }
         25%, 75% {
           opacity: 0;
         }
       }
       @keyframes z5h_timeline__flash__1 {
         from, 50%, to {
           opacity: 1;
         }
         25%, 75% {
           opacity: 0;
         }
       }
       /*=== crossfade ===*/
       @keyframes z5h_timeline__crossfade__0 {
         from {
           opacity: 0;
         }
         to {
           opacity: 1;
         }
       }
       @keyframes z5h_timeline__crossfade__1 {
         from {
           opacity: 0;
         }
         to {
           opacity: 1;
         }
       }

       /*# sourceMappingURL=keyframes.css.map */
"""
