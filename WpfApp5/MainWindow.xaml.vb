Option Explicit On
Option Strict On

Imports System.Windows.Media.Media3D
Imports HelixToolkit.Wpf

Class MainWindow
    Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
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

        diffuseMaterial = New DiffuseMaterial(New SolidColorBrush(GetDiffuse()))

        emissiveMaterial = New EmissiveMaterial(New SolidColorBrush(GetEmissive()))

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

        headlight = New DirectionalHeadLight() With {
            .Brightness = SliderBrightness.Value
        }
        Viewport.Children.Add(headlight)
    End Sub

    Private headlight As DirectionalHeadLight
    Private diffuseMaterial As DiffuseMaterial
    Private emissiveMaterial As EmissiveMaterial

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

    Private Sub SliderColor_ValueChanged(sender As Object, e As RoutedPropertyChangedEventArgs(Of Double))
        Dim slider = DirectCast(sender, Slider)

        If diffuseMaterial IsNot Nothing Then
            If ChkSingleColor.IsChecked AndAlso (slider Is SliderDiffuseR OrElse slider Is SliderDiffuseG OrElse slider Is SliderDiffuseB) Then
                SliderDiffuseR.Value = slider.Value
                SliderDiffuseG.Value = slider.Value
                SliderDiffuseB.Value = slider.Value
            End If
            diffuseMaterial.Color = GetDiffuse()
        End If

        If emissiveMaterial IsNot Nothing Then
            If ChkSingleColor.IsChecked AndAlso (slider Is SliderEmissiveR OrElse slider Is SliderEmissiveG OrElse slider Is SliderEmissiveB) Then
                SliderEmissiveR.Value = slider.Value
                SliderEmissiveG.Value = slider.Value
                SliderEmissiveB.Value = slider.Value
            End If
            emissiveMaterial.Color = GetEmissive()
        End If
    End Sub

    Private Function GetDiffuse() As Color
        Return Color.FromArgb(255, CByte(SliderDiffuseR.Value), CByte(SliderDiffuseG.Value), CByte(SliderDiffuseB.Value))
    End Function

    Private Function GetEmissive() As Color
        Return Color.FromArgb(255, CByte(SliderEmissiveR.Value), CByte(SliderEmissiveG.Value), CByte(SliderEmissiveB.Value))
    End Function

    Private Sub Window_Loaded(sender As Object, e As RoutedEventArgs)
        Viewport.ZoomExtents()
    End Sub

    Private Sub ChkSingleColor_Checked(sender As Object, e As RoutedEventArgs)
        SliderDiffuseG.Value = SliderDiffuseR.Value
        SliderDiffuseB.Value = SliderDiffuseR.Value

        SliderEmissiveG.Value = SliderEmissiveR.Value
        SliderEmissiveB.Value = SliderEmissiveR.Value
    End Sub

    Private Sub SliderBrightness_ValueChanged(sender As Object, e As RoutedPropertyChangedEventArgs(Of Double))
        If headlight IsNot Nothing Then
            headlight.Brightness = SliderBrightness.Value
        End If
    End Sub
End Class
