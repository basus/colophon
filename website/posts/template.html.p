<!DOCTYPE html>
<html lang="en">
  ◊(->html (head-with #:theme "/css/grayscale.css"))
  ◊(->html (body-with
            #:theme-variant "light"
            #:contents `(article ((id "content"))
                                 ,@(make-top metas)
                                 ,@(select* 'root doc))))
</html>
