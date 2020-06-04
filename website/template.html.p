<!DOCTYPE html>
<html lang="en">
  ◊(->html (default-head))
  ◊(->html (body-with
            #:class "theme-dark"
            #:contents `(div ((id "content"))
                             ,@(select* 'root doc))))

</html>
