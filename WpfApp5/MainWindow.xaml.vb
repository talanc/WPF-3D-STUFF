Option Explicit On
Option Strict On

Imports System.Windows.Media.Media3D
Imports HelixToolkit.Wpf

Class MainWindow
    Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        Dim lines As New LinesVisual3D With {
            .Thickness = 1,
            .DepthOffset = 0.0001
        }

        diffuseBrush = New SolidColorBrush(Colors.Gray)
        diffuseMaterial = New DiffuseMaterial(diffuseBrush)
        emissiveBrush = New SolidColorBrush(Colors.Gray)
        emissiveMaterial = New EmissiveMaterial(emissiveBrush)

        headlight = New DirectionalHeadLight() With {
            .Brightness = 1.0
        }

        Dim numTrisLabel As New Label()

        Dim createSeparator = Function() New Separator() With {.Margin = New Thickness(10)}
        Dim createByteSlider = Function(value As Byte) New Slider() With {.Minimum = 0, .Maximum = 255, .Value = value}

        Dim addColorSlider As Action(Of String, SolidColorBrush) =
            Sub(name As String, brush As SolidColorBrush)
                Dim stackPanel As New StackPanel() With {
                    .Margin = New Thickness(4)
                }

                Dim header As New StackPanel() With {.Orientation = Orientation.Horizontal}
                Dim label As New Label() With {.Content = name}
                Dim swatch As New Rectangle() With {
                    .Width = 12,
                    .Height = 12,
                    .Fill = brush,
                    .Stroke = Brushes.Black,
                    .StrokeThickness = 1
                }
                header.Children.Add(swatch)
                header.Children.Add(label)

                Dim sliderR = createByteSlider(brush.Color.R)
                Dim sliderG = createByteSlider(brush.Color.G)
                Dim sliderB = createByteSlider(brush.Color.B)
                Dim checkbox As New CheckBox() With {
                    .Content = "Single Color",
                    .IsChecked = True
                }
                Dim separator = createSeparator()

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
                                             brush.Color = newColor
                                         End Sub

                AddHandler sliderR.ValueChanged, sliderValueChanged
                AddHandler sliderG.ValueChanged, sliderValueChanged
                AddHandler sliderB.ValueChanged, sliderValueChanged

                stackPanel.Children.Add(header)
                stackPanel.Children.Add(sliderR)
                stackPanel.Children.Add(sliderG)
                stackPanel.Children.Add(sliderB)
                stackPanel.Children.Add(checkbox)
                stackPanel.Children.Add(separator)
                Tools.Children.Add(stackPanel)
            End Sub

        addColorSlider("Diffuse", diffuseBrush)
        addColorSlider("Emissive", emissiveBrush)

        If True Then
            Dim stackPanel As New StackPanel()
            Dim label As New Label() With {.Content = "Brightness"}
            Dim slider As New Slider() With {
                .Minimum = 0,
                .Maximum = 1,
                .Value = headlight.Brightness
            }
            Dim separator = createSeparator()

            AddHandler slider.ValueChanged, Sub() headlight.Brightness = slider.Value

            stackPanel.Children.Add(label)
            stackPanel.Children.Add(slider)
            stackPanel.Children.Add(separator)
            Tools.Children.Add(stackPanel)
        End If

        If True Then
            Dim stackPanel As New StackPanel()

            Dim showFps As New CheckBox() With {
                .Content = "Show FPS"
            }
            AddHandler showFps.Checked, Sub() Viewport.ShowFrameRate = True
            AddHandler showFps.Unchecked, Sub() Viewport.ShowFrameRate = False

            Dim showLines As New CheckBox() With {
                .Content = "Show Lines",
                .IsChecked = True
            }
            AddHandler showLines.Checked, Sub() Viewport.Children.Add(lines)
            AddHandler showLines.Unchecked, Sub() Viewport.Children.Remove(lines)

            stackPanel.Children.Add(numTrisLabel)
            stackPanel.Children.Add(showFps)
            stackPanel.Children.Add(showLines)
            Tools.Children.Add(stackPanel)
        End If

        Dim mesh As New MeshGeometry3D()

        Dim lineSegments As New List(Of Point3D)

        Dim c15024 As New CeeInfo() With {.Web = 152, .Flange = 64, .Lip = 18.5, .Thickness = 2.4}

        If True Then
            Dim col1Mat = Matrix3D.Identity
            col1Mat.Translate(V3(0, -152 / 2, 0))

            Dim col2Mat = Matrix3D.Identity
            col2Mat.Translate(V3(0, 500 + 152 / 2, 0))

            Dim beamMat = Matrix3D.Identity
            beamMat.Rotate(New Quaternion(V3(1, 0, 0), -90))
            beamMat.Translate(V3(0, 0, 500 - 152 / 2))

            For Each mat In {col1Mat, col2Mat, beamMat}
                AddCee(mesh, lineSegments, mat, 500, c15024)
            Next
        End If

        If True Then
            Dim colStart = P3(1000, 1000, 0)
            Dim colEnd = colStart + V3(0, 0, 1000)
            Dim colXDir = V3(1, 0, 0)
            AddCee(mesh, lineSegments, c15024, colStart, colEnd, colXDir)
        End If

        If True Then
            Dim rnd As New Random()
            For i As Integer = 1 To 500
                Dim p1 = P3(rnd.NextDouble() * 10000, rnd.NextDouble() * 10000, 0)
                Dim p2 = p1 + V3(0, 0, 500 + rnd.NextDouble() * 1000)
                Dim ang = rnd.Next(0, 360) / (Math.PI * 2)
                Dim xdir = V3(Math.Cos(ang), Math.Sin(ang), 0)
                AddCee(mesh, lineSegments, c15024, p1, p2, xdir)
            Next
        End If

        Dim material As New MaterialGroup()
        material.Children.Add(diffuseMaterial)
        material.Children.Add(emissiveMaterial)

        Dim model As New GeometryModel3D(mesh, material)

        Dim visual As New ModelVisual3D With {
            .Content = model
        }

        lines.Points = New Point3DCollection(lineSegments)

        Viewport.Children.Add(visual)
        Viewport.Children.Add(lines)
        Viewport.Children.Add(headlight)

        Dim numTris = 0
        Viewport.Children.Traverse(Of GeometryModel3D)(
            Sub(m, v, t)
                Dim geometry = TryCast(m.Geometry, MeshGeometry3D)
                If geometry IsNot Nothing Then
                    If geometry.TriangleIndices?.Count > 0 Then
                        numTris += geometry.TriangleIndices.Count \ 3
                    ElseIf geometry.Positions?.Count > 0 Then
                        numTris += geometry.Positions.Count \ 3
                    End If
                End If
            End Sub)

        numTrisLabel.Content = $"Num Tris: {numTris}"
    End Sub

    Private headlight As DirectionalHeadLight
    Private diffuseMaterial As DiffuseMaterial
    Private emissiveMaterial As EmissiveMaterial
    Private diffuseBrush As SolidColorBrush
    Private emissiveBrush As SolidColorBrush

    Class CeeInfo
        Public Web As Double
        Public Flange As Double
        Public Lip As Double
        Public Thickness As Double
    End Class

    Private Sub AddCee(mesh As MeshGeometry3D, linePoints As IList(Of Point3D), cee As CeeInfo, startPos As Point3D, endPos As Point3D, xdir As Vector3D)
        Dim spine = endPos - startPos
        Dim len = spine.Length
        Dim zdir = spine / len
        Dim ydir = Vector3D.CrossProduct(zdir, xdir)

        Dim mat As New Matrix3D(
            xdir.X, xdir.Y, xdir.Z, 0,
            ydir.X, ydir.Y, ydir.Z, 0,
            zdir.X, zdir.Y, zdir.Z, 0,
            startPos.X, startPos.Y, startPos.Z, 1
        )

        AddCee(mesh, linePoints, mat, len, cee)
    End Sub

    Private Sub AddCee(mesh As MeshGeometry3D, linePoints As IList(Of Point3D), mat As Matrix3D, len As Double, cee As CeeInfo)
        Dim hf = cee.Flange / 2
        Dim hw = cee.Web / 2
        Dim l = cee.Lip
        Dim t = cee.Thickness

        Dim positions As Point3D() = {
            P3(+hf, +hw - l, 0),
            P3(+hf, +hw, 0),
            P3(-hf, +hw, 0),
            P3(-hf, -hw, 0),
            P3(+hf, -hw, 0),
            P3(+hf, -hw + l, 0),
            P3(+hf - t, -hw + l, 0),
            P3(+hf - t, -hw + t, 0),
            P3(-hf + t, -hw + t, 0),
            P3(-hf + t, +hw - t, 0),
            P3(+hf - t, +hw - t, 0),
            P3(+hf - t, +hw - l, 0),
            P3(+hf, +hw - l, len),
            P3(+hf, +hw, len),
            P3(-hf, +hw, len),
            P3(-hf, -hw, len),
            P3(+hf, -hw, len),
            P3(+hf, -hw + l, len),
            P3(+hf - t, -hw + l, len),
            P3(+hf - t, -hw + t, len),
            P3(-hf + t, -hw + t, len),
            P3(-hf + t, +hw - t, len),
            P3(+hf - t, +hw - t, len),
            P3(+hf - t, +hw - l, len)
        }

        Dim lineOffset = 0.0
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

        For i = 0 To positions.Length - 1
            positions(i) = mat.Transform(positions(i))
            linePositions(i) = mat.Transform(linePositions(i))
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

        Dim line =
            Sub(a As Integer, b As Integer)
                linePoints.Add(linePositions(a))
                linePoints.Add(linePositions(b))
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
    End Sub

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
