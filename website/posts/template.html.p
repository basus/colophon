<!DOCTYPE html>
<html lang="en">
  ◊(->html (default-head))
  ◊(->html (body-with
            #:class "theme-light"
            #:contents `(article ((id "content"))
                                 ,@(make-top metas)
                                 ,@(select* 'root doc))))

</html>
