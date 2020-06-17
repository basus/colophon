<!DOCTYPE html>
<html lang="en">
  ◊(->html (head-with #:theme "/css/grayscale.css"))
  ◊(->html (body-with
            #:theme-variant "light"
            #:contents `(div ((id "content"))
                             ,@(select* 'root doc))))

</html>
