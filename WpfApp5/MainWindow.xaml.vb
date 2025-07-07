Option Explicit On
Option Strict On

Imports System.Windows.Media.Media3D
Imports HelixToolkit.Wpf

Class MainWindow
    Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        diffuseBrush = New SolidColorBrush(Colors.Gray)
        diffuseMaterial = New DiffuseMaterial(diffuseBrush)
        emissiveBrush = New SolidColorBrush(Colors.Gray)
        emissiveMaterial = New EmissiveMaterial(emissiveBrush)

        headlight = New DirectionalHeadLight() With {
            .Brightness = 1.0
        }

        Dim addColorSlider As Action(Of String, Color, Action(Of Color)) =
            Sub(name As String, initialColor As Color, update As Action(Of Color))
                Dim stackPanel As New StackPanel()
                Dim label As New Label() With {.Content = name}
                Dim sliderR As New Slider() With {.Minimum = 0, .Maximum = 255, .Value = initialColor.R}
                Dim sliderG As New Slider() With {.Minimum = 0, .Maximum = 255, .Value = initialColor.G}
                Dim sliderB As New Slider() With {.Minimum = 0, .Maximum = 255, .Value = initialColor.B}
                Dim checkbox As New CheckBox() With {
                    .Content = "Single Color",
                    .IsChecked = True
                }
                Dim separator As New Separator() With {.Margin = New Thickness(10)}

                AddHandler checkbox.Checked, Sub()
                                                 sliderG.Value = sliderR.Value
                                                 sliderB.Value = sliderR.Value
                                             End Sub

                Dim sliderValueChanged = Sub(sender As Object, e As RoutedPropertyChangedEventArgs(Of Double))
                                             Dim slider = DirectCast(sender, Slider)
                                             If checkbox.IsChecked Then
                                                 sliderR.Value = slider.Value
                                                 sliderG.Value = slider.Value
                                                 sliderB.Value = slider.Value
                                             End If
                                             Dim newColor As Color = Color.FromArgb(255, CByte(sliderR.Value), CByte(sliderG.Value), CByte(sliderB.Value))
                                             update(newColor)
                                         End Sub

                AddHandler sliderR.ValueChanged, sliderValueChanged
                AddHandler sliderG.ValueChanged, sliderValueChanged
                AddHandler sliderB.ValueChanged, sliderValueChanged

                stackPanel.Children.Add(label)
                stackPanel.Children.Add(sliderR)
                stackPanel.Children.Add(sliderG)
                stackPanel.Children.Add(sliderB)
                stackPanel.Children.Add(checkbox)
                stackPanel.Children.Add(separator)
                Tools.Children.Add(stackPanel)
            End Sub

        addColorSlider("Diffuse (Material)", diffuseMaterial.Color, Sub(c) diffuseMaterial.Color = c)
        addColorSlider("Diffuse (Brush)", diffuseBrush.Color, Sub(c) diffuseBrush.Color = c)
        addColorSlider("Emissive (Material)", emissiveMaterial.Color, Sub(c) emissiveMaterial.Color = c)
        addColorSlider("Emissive (Brush)", emissiveBrush.Color, Sub(c) emissiveBrush.Color = c)

        If True Then
            Dim stackPanel As New StackPanel()
            Dim label As New Label() With {.Content = "Brightness"}
            Dim slider As New Slider() With {
                .Minimum = 0,
                .Maximum = 1,
                .Value = headlight.Brightness
            }
            Dim separator As New Separator() With {.Margin = New Thickness(10)}

            AddHandler slider.ValueChanged, Sub() headlight.Brightness = slider.Value

            stackPanel.Children.Add(label)
            stackPanel.Children.Add(slider)
            stackPanel.Children.Add(separator)
            Tools.Children.Add(stackPanel)
        End If

        Dim w = 152.0
        Dim f = 64.0
        Dim l = 18.5
        Dim t = 2.4

        Dim len = 500.0

        Dim hf = f / 2
        Dim hw = w / 2

        Dim mesh As New MeshGeometry3D()

        Dim positions As New List(Of Point3D)

        positions.Add(P3(+hf, +hw - l, 0))
        positions.Add(P3(+hf, +hw, 0))
        positions.Add(P3(-hf, +hw, 0))
        positions.Add(P3(-hf, -hw, 0))
        positions.Add(P3(+hf, -hw, 0))
        positions.Add(P3(+hf, -hw + l, 0))

        positions.Add(positions(5) + V3(-t, 0, 0))
        positions.Add(positions(4) + V3(-t, +t, 0))
        positions.Add(positions(3) + V3(+t, +t, 0))
        positions.Add(positions(2) + V3(+t, -t, 0))
        positions.Add(positions(1) + V3(-t, -t, 0))
        positions.Add(positions(0) + V3(-t, 0, 0))

        For i = 0 To 11
            positions.Add(positions(i) + V3(0, 0, len))
        Next

        Dim tri =
            Sub(a As Integer, b As Integer, c As Integer)
                mesh.Positions.Add(positions(a))
                mesh.Positions.Add(positions(b))
                mesh.Positions.Add(positions(c))
            End Sub

        Dim quad =
            Sub(a As Integer, b As Integer, c As Integer, d As Integer)
                tri(b, c, a)
                tri(d, a, c)
            End Sub

        ' bottom
        quad(11, 10, 1, 0)
        quad(10, 9, 2, 1)
        quad(9, 8, 3, 2)
        quad(8, 7, 4, 3)
        quad(7, 6, 5, 4)

        ' top
        quad(12, 13, 22, 23)
        quad(13, 14, 21, 22)
        quad(14, 15, 20, 21)
        quad(15, 16, 19, 20)
        quad(16, 17, 18, 19)

        ' sides
        quad(0, 1, 13, 12)
        quad(1, 2, 14, 13)
        quad(2, 3, 15, 14)
        quad(3, 4, 16, 15)
        quad(4, 5, 17, 16)
        quad(5, 6, 18, 17)
        quad(6, 7, 19, 18)
        quad(7, 8, 20, 19)
        quad(8, 9, 21, 20)
        quad(9, 10, 22, 21)
        quad(10, 11, 23, 22)
        quad(11, 0, 12, 23)

        Dim material As New MaterialGroup()
        material.Children.Add(diffuseMaterial)
        material.Children.Add(emissiveMaterial)

        Dim model As New GeometryModel3D With {
            .Geometry = mesh,
            .Material = material
        }

        Dim visual As New ModelVisual3D With {
            .Content = model
        }

        Viewport.Children.Add(visual)

        Dim lines As New LinesVisual3D With {
            .Thickness = 1
        }

        Dim lineOffset = 0.3
        Dim linePositions = positions.ToList()
        For i = 0 To linePositions.Count - 1
            Dim newPos = linePositions(i)

            Select Case newPos.X
                Case -hf, +hf - t
                    newPos.X -= lineOffset
                Case +hf, -hf + t
                    newPos.X += lineOffset
                Case Else
                    Throw New InvalidOperationException($"Unknown X: {newPos.X}")
            End Select

            Select Case newPos.Y
                Case -hw, +hw - l, +hw - t
                    newPos.Y -= lineOffset
                Case +hw, -hw + l, -hw + t
                    newPos.Y += lineOffset
                Case Else
                    Throw New InvalidOperationException($"Unknown Y: {newPos.Y}")
            End Select

            Select Case newPos.Z
                Case 0
                    newPos.Z -= lineOffset
                Case len
                    newPos.Z += lineOffset
                Case Else
                    Throw New InvalidOperationException($"Unknown Z: {newPos.Z}")
            End Select

            linePositions(i) = newPos
        Next

        Dim line =
            Sub(a As Integer, b As Integer)
                lines.Points.Add(linePositions(a))
                lines.Points.Add(linePositions(b))
            End Sub

        Dim lineRangeLoop =
            Sub(a As Integer, b As Integer)
                For i = a + 1 To b
                    line(i - 1, i)
                Next
                line(b, a)
            End Sub

        lineRangeLoop(0, 11)
        lineRangeLoop(12, 23)
        For i = 0 To 11
            line(i, i + 12)
        Next

        Viewport.Children.Add(lines)

        'Viewport.Children.Add(New SunLight())

        Viewport.Children.Add(headlight)
    End Sub

    Private headlight As DirectionalHeadLight
    Private diffuseMaterial As DiffuseMaterial
    Private emissiveMaterial As EmissiveMaterial
    Private diffuseBrush As SolidColorBrush
    Private emissiveBrush As SolidColorBrush

    Private Function P3(x As Double, y As Double, z As Double) As Point3D
        Return New Point3D(x, y, z)
    End Function

    Private Function V3(x As Double, y As Double, z As Double) As Vector3D
        Return New Vector3D(x, y, z)
    End Function

    Private Sub Viewport_MouseDoubleClick(sender As Object, e As MouseButtonEventArgs)
        Dim pos = Viewport3DHelper.FindNearestPoint(Viewport.Viewport, e.GetPosition(Viewport))
        If pos.HasValue Then
            Viewport.Camera.LookAt(pos.Value, 500)
        End If
    End Sub

    Private Sub Window_Loaded(sender As Object, e As RoutedEventArgs)
        Viewport.ZoomExtents()
    End Sub
End Class
