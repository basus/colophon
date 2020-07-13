<!DOCTYPE html>
<html lang="en">
  ◊(->html (head-with #:theme "/css/grayscale.css"))
  ◊(->html (body-with
            #:theme-variant "light"
            #:contents `(article ((id "content"))
                                 ,@(make-top metas)
                                 ,@(remove-more doc))))

  <footer>
    <p>
      &copy; Shrutarshi Basu 2020. Unless otherwise noted, this work is licensed
      under a
      <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" rel="license">
        Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
    </p>
  </footer>

</html>
