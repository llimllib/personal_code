Attribute VB_Name = "Chapter08"
Option Explicit
' Global variable
Dim pres As Presentation

' Listing 8.1. This procedure ties everything together by
' calling each of the code listings individually.
'
Sub Main()
    '
    ' Create the presentation file
    '
    CreateJugglingPresentation
    '
    ' Add the slides
    '
    AddJugglingSlides
    '
    ' Set up the title page
    '
    SetUpStartPage
    '
    ' Set up the Juggling pages
    '
    SetUpJugglingPages
    '
    ' Save it and then run it
    '
    pres.Save
    RunJugglingSlideShow
End Sub

' Listing 8.2. This procedure creates a new presentation
' and then saves it.
'
Sub CreateJugglingPresentation()
    Dim p As Presentation
    '
    ' If the old one is still open, close it without saving
    '
    For Each p In Presentations
        If p.Name = "Juggling" Then
            p.Saved = True
            p.Close
        End If
    Next p
    '
    ' Create a new Presentation object and store it in pres
    '
    Set pres = Presentations.Add
    pres.SaveAs FileName:="Juggling.ppt"
End Sub

' Listing 8.3. A procedure that adds the slides to the
' Juggling presentation and formats them.
'
Sub AddJugglingSlides()
    Dim i As Integer
    
    With pres
        With .Slides
         '
         ' Add the opening slide
         '
         .Add(Index:=1, Layout:=ppLayoutTitle).Name = "Opener"
         '
         ' Now add the slides for each step
         '
         For i = 1 To 4
             .Add(Index:=i + 1, Layout:=ppLayoutTitle).Name = "Juggling" & i
         Next i
    End With
    '
    ' Set the background for all the slides
    '
    .SlideMaster.Background.Fill.PresetGradient _
        Style:=msoGradientHorizontal, _
        Variant:=1, _
        PresetGradientType:=msoGradientNightfall
    End With
End Sub

' Listing 8.4. A procedure that sets up the text and animation
' settings for the first page of the Juggling presentation.
'
Sub SetUpStartPage()
    Dim shapeTitle As Shape
    Dim shapeSubTitle As Shape
    
    With pres.Slides("Opener")
        Set shapeTitle = .Shapes(1)     ' The title
        Set shapeSubTitle = .Shapes(2)  ' The subtitle
        '
        ' Add the title text
        '
        With shapeTitle.TextFrame.TextRange
            .Text = "Juggling"
            With .Font
                .Name = "Arial"
                .Size = 44
                .Bold = True
                .Color.RGB = RGB(255, 255, 255)
            End With
        End With
        '
        ' Set the title animation
        '
        With shapeTitle.AnimationSettings
            .Animate = True
            .AdvanceMode = ppAdvanceOnTime
            .AdvanceTime = 0
            .TextUnitEffect = ppAnimateByCharacter
            .EntryEffect = ppEffectFlyFromLeft
        End With
        '
        ' Add the subtitle text
        '
        With shapeSubTitle.TextFrame.TextRange
            .Text = "A Step-By-Step Course"
            With .Font
                .Name = "Arial"
                .Size = 36
                .Bold = True
                .Color.RGB = RGB(255, 255, 255)
            End With
        End With
        '
        ' Set the subtitle animation
        '
        With shapeSubTitle.AnimationSettings
            .Animate = True
            .AdvanceMode = ppAdvanceOnTime
            .AdvanceTime = 0
            .TextUnitEffect = ppAnimateByWord
            .EntryEffect = ppEffectFlyFromBottom
        End With
    End With
End Sub

' Listing 8.5. A procedure that sets up the titles, pictures,
' and text instructions for each of the Juggling slides.
'
Sub SetUpJugglingPages()

    Dim thisPres As Presentation
    Dim slideTitle As Shape
    Dim slidePicture As Shape
    Dim slideText As Shape
    Dim i As Integer
    
    For i = 1 To 4
        With pres.Slides("Juggling" & i)
            '
            ' Get pictures from Chaptr08.ppt
            '
            Set thisPres = Presentations("Chaptr08.ppt")
            thisPres.Slides(1).Shapes(i + 1).Copy
            .Shapes.Paste
            '
            ' Adjust the layout and then set the Shape variables
            '
            .Layout = ppLayoutObjectOverText
            Set slideTitle = .Shapes(1)
            Set slidePicture = .Shapes(2)
            Set slideText = .Shapes(3)
            '
            ' Add the title text
            '
            With slideTitle.TextFrame.TextRange
                Select Case i
                    Case 1
                        .Text = "Step 1: The Home Position"
                    Case 2
                        .Text = "Step 2: The First Throw"
                    Case 3
                        .Text = "Step 3: The Second Throw"
                    Case 4
                        .Text = "Step 4: The Third Throw"
                End Select
                With .Font
                    .Name = "Arial"
                    .Size = 44
                    .Bold = True
                    .Color.RGB = RGB(255, 255, 255)
                End With
            End With
            '
            ' Set the picture animation and shadow
            '
            With slidePicture
                With .AnimationSettings
                    .Animate = True
                    .AdvanceMode = ppAdvanceOnTime
                    .AdvanceTime = 0
                    .EntryEffect = ppEffectFade
                End With
                With .Shadow
                    .ForeColor.RGB = RGB(0, 0, 0)
                    .OffsetX = 10
                    .OffsetY = 10
                    .Visible = True
                End With
            End With
            '
            ' Add the instruction text
            '
            With slideText.TextFrame.TextRange
                Select Case i
                    Case 1
                        .Text = "Place two balls in your dominant hand, " & _
                                "one in front of the other." & Chr(13) & _
                                "Hold the third ball in your other hand." & Chr(13) & _
                                "Let your arms dangle naturally and bring your " & _
                                "forearms parallel to the ground (as though you " & _
                                "were holding a tray.)" & Chr(13) & _
                                "Relax your shoulders, arms, and hands."
                    Case 2
                        .Text = "Of the two balls in your dominant hand, " & _
                                "toss the front one towards your other hand " & _
                                "in a smooth arc." & Chr(13) & _
                                "Make sure the ball doesn't spin too much." & Chr(13) & _
                                "Make sure the ball goes no higher than about eye level."
                    Case 3
                        .Text = "Once the first ball reaches the top of its arc, " & _
                                "toss the ball in your other hand." & Chr(13) & _
                                "Throw the ball towards your dominant hand, making " & _
                                "sure that it flies UNDER the first ball." & Chr(13) & _
                                "Again, try not to spin the ball and make sure it goes " & _
                                "no higher than eye level."
                    Case 4
                        .Text = "Now for the tricky part (!). Soon after you release " & _
                                "the second ball, the first ball will approach your " & _
                                "hand. Go ahead and catch the first ball." & Chr(13) & _
                                "When the second ball reaches its apex, throw the " & _
                                "third ball (the remaining ball in your dominant hand) " & _
                                "under it." & Chr(13) & _
                                "At this point, it just becomes a game of catch-and-" & _
                                "throw-under, catch-and-throw-under. Have fun!"
                End Select
                With .Font
                    .Name = "Times New Roman"
                    .Size = 24
                    .Bold = False
                    .Color.RGB = RGB(255, 255, 255)
                End With
            End With
        End With
    Next i
    
End Sub

' Listing 8.6. This procedure asks the user if they want to
' run the presentation's slide show.
'
Sub RunJugglingSlideShow()
    If MsgBox("Start the slide show?", vbYesNo, "Juggling") = vbYes Then
        With pres
            .Slides("Juggling1").SlideShowTransition.EntryEffect = ppEffectBlindsHorizontal
            .Slides("Juggling2").SlideShowTransition.EntryEffect = ppEffectCheckerboardAcross
            .Slides("Juggling3").SlideShowTransition.EntryEffect = ppEffectBoxIn
            .Slides("Juggling4").SlideShowTransition.EntryEffect = ppEffectStripsLeftDown
            .SlideShowSettings.Run
        End With
    End If
End Sub

