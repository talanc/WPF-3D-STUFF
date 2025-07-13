Option Explicit On
Option Strict On

Imports HelixToolkit.Wpf
Imports System.Windows.Media.Media3D

Class MainWindow
    Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        Dim diffuseBrush As New SolidColorBrush(Colors.Gray)
        Dim diffuseMaterial As New DiffuseMaterial(diffuseBrush)
        Dim emissiveBrush As New SolidColorBrush(Colors.Gray)
        Dim emissiveMaterial As New EmissiveMaterial(emissiveBrush)

        Dim ceeMaterial As New MaterialGroup()
        ceeMaterial.Children.Add(diffuseMaterial)
        ceeMaterial.Children.Add(emissiveMaterial)

        Dim modelVisuals As ModelVisual3D = Nothing

        Dim headlight As New DirectionalHeadLight() With {
            .Brightness = 1.0
        }
        Viewport.Children.Add(headlight)

        Dim opt = GenOpt.Opt1

        Dim numVertsLabel, numTrisLabel, numMeshLabel, geoTimeLabel As New Label

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
                stackPanel.Children.Add(createSeparator())
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

            AddHandler slider.ValueChanged, Sub() headlight.Brightness = slider.Value

            stackPanel.Children.Add(label)
            stackPanel.Children.Add(slider)
            stackPanel.Children.Add(createSeparator())
            Tools.Children.Add(stackPanel)
        End If

        If True Then
            Dim stackPanel As New StackPanel()

            Dim showFps As New CheckBox() With {
                .Content = "Show FPS",
                .IsChecked = Viewport.ShowFrameRate
            }
            AddHandler showFps.Checked, Sub() Viewport.ShowFrameRate = True
            AddHandler showFps.Unchecked, Sub() Viewport.ShowFrameRate = False

            Dim clipToBounds As New CheckBox() With {
                .Content = "Clip to bounds",
                .IsChecked = Viewport.ClipToBounds
            }
            AddHandler clipToBounds.Checked, Sub() Viewport.ClipToBounds = True
            AddHandler clipToBounds.Unchecked, Sub() Viewport.ClipToBounds = False

            stackPanel.Children.Add(numVertsLabel)
            stackPanel.Children.Add(numTrisLabel)
            stackPanel.Children.Add(numMeshLabel)
            stackPanel.Children.Add(geoTimeLabel)
            stackPanel.Children.Add(showFps)
            stackPanel.Children.Add(clipToBounds)
            stackPanel.Children.Add(createSeparator())
            Tools.Children.Add(stackPanel)
        End If

        Dim resetMetrics =
            Sub()
                Dim numVerts = 0
                Dim numTris = 0
                Dim numMeshes = 0
                Viewport.Children.Traverse(Of GeometryModel3D)(
                    Sub(m, v, t)
                        Dim geometry = TryCast(m.Geometry, MeshGeometry3D)
                        If geometry IsNot Nothing Then
                            If geometry.TriangleIndices?.Count > 0 Then
                                numVerts += geometry.Positions.Count
                                numTris += geometry.TriangleIndices.Count \ 3
                            ElseIf geometry.Positions?.Count > 0 Then
                                numVerts += geometry.Positions.Count
                                numTris += geometry.Positions.Count \ 3
                            End If
                            numMeshes += 1
                        End If
                    End Sub)

                numVertsLabel.Content = $"Num Verts: {numVerts}"
                numTrisLabel.Content = $"Num Tris: {numTris}"
                numMeshLabel.Content = $"Num Meshes: {numMeshes}"
                geoTimeLabel.Content = $"Create Time: {sw.ElapsedMilliseconds}ms"
            End Sub

        Dim showSimple, showFrame, showFar As New CheckBox
        Dim forestNum, forestSize As New ComboBox
        Dim optMesh1, optMesh2, optMesh3 As New RadioButton

        Dim resetGeometry =
            Sub()
                sw.Restart()

                Dim posLists As New List(Of Point3DCollection)

                If showFrame.IsChecked Then
                    Dim col1Mat = Matrix3D.Identity
                    col1Mat.Translate(V3(0, -152 / 2, 0))

                    Dim col2Mat = Matrix3D.Identity
                    col2Mat.Translate(V3(0, 500 + 152 / 2, 0))

                    Dim beamMat = Matrix3D.Identity
                    beamMat.Rotate(New Quaternion(V3(1, 0, 0), -90))
                    beamMat.Translate(V3(0, 0, 500 - 152 / 2))

                    For Each mat In {col1Mat, col2Mat, beamMat}
                        AddCee(posLists, opt, mat, 500, c15024)
                    Next
                End If

                If showSimple.IsChecked Then
                    Dim colStart = P3(1000, 1000, 0)
                    Dim colEnd = colStart + V3(0, 0, 1000)
                    Dim colXDir = V3(1, 0, 0)
                    AddCee(posLists, opt, c15024, colStart, colEnd, colXDir)
                End If

                Dim numForest = 0
                Integer.TryParse(forestNum.SelectedItem.ToString(), numForest)
                Dim sizeForest = 1000 * Double.Parse(forestSize.SelectedItem.ToString())
                For i As Integer = 1 To numForest
                    Dim p1 = P3(rnd.NextDouble() * sizeForest, rnd.NextDouble() * sizeForest, 0)
                    Dim p2 = p1 + V3(0, 0, 500 + rnd.NextDouble() * 2000)
                    Dim ang = rnd.Next(0, 360) / (Math.PI * 2)
                    Dim xdir = V3(Math.Cos(ang), Math.Sin(ang), 0)
                    AddCee(posLists, opt, c15024, p1, p2, xdir)
                Next

                If showFar.IsChecked Then
                    Dim p1 = P3(100000, 100000, 0)
                    Dim p2 = p1 + V3(0, 0, 1000)
                    Dim xdir = V3(1, 0, 0)
                    AddCee(posLists, opt, c15024, p1, p2, xdir)
                End If

                Dim modelGroup As New Model3DGroup
                For Each posList In posLists
                    Dim mesh As New MeshGeometry3D() With {
                        .Positions = posList
                    }
                    Dim model As New GeometryModel3D(mesh, ceeMaterial)
                    modelGroup.Children.Add(model)
                Next

                If modelVisuals IsNot Nothing Then
                    Viewport.Children.Remove(modelVisuals)
                End If

                modelVisuals = New ModelVisual3D With {
                    .Content = modelGroup
                }
                Viewport.Children.Add(modelVisuals)

                sw.Stop()

                resetMetrics()
            End Sub

        If True Then
            Dim stackPanel As New StackPanel

            Dim addCheckBox =
                Function(checkBox As CheckBox, name As String, isChecked As Boolean)
                    checkBox.Content = name
                    checkBox.IsChecked = isChecked
                    AddHandler checkBox.Checked, Sub() resetGeometry()
                    AddHandler checkBox.Unchecked, Sub() resetGeometry()
                    stackPanel.Children.Add(checkBox)
                    Return checkBox
                End Function
            addCheckBox(showSimple, "Show Simple", True)
            addCheckBox(showFrame, "Show Frame", False)
            addCheckBox(showFar, "Show Far", False)

            Dim forestGrid As New Grid
            forestGrid.ColumnDefinitions.Add(New ColumnDefinition With {.Width = GridLength.Auto})
            forestGrid.ColumnDefinitions.Add(New ColumnDefinition With {.Width = New GridLength(1, GridUnitType.Star)})

            Dim addForestCombo =
                Sub(name As String, combo As ComboBox, items As String(), defaultItem As String)
                    Dim row = forestGrid.RowDefinitions.Count
                    forestGrid.RowDefinitions.Add(New RowDefinition())

                    Dim label As New Label With {
                        .Content = name
                    }

                    For Each item In items
                        combo.Items.Add(item)
                    Next
                    combo.SelectedItem = defaultItem
                    AddHandler combo.SelectionChanged, Sub() resetGeometry()

                    forestGrid.Children.Add(label)
                    forestGrid.Children.Add(combo)

                    Grid.SetRow(label, row)
                    Grid.SetColumn(forestGrid, 0)

                    Grid.SetRow(combo, row)
                    Grid.SetColumn(combo, 1)
                End Sub
            addForestCombo("Forest Num", forestNum, {"None", "100", "1000", "10000", "100000", "1000000"}, "None")
            addForestCombo("Forest Size", forestSize, {"10", "100", "1000", "10000"}, "10")

            stackPanel.Children.Add(forestGrid)
            stackPanel.Children.Add(createSeparator())
            Tools.Children.Add(stackPanel)
        End If

        If True Then
            Dim stackPanel As New StackPanel

            Dim addOpt =
                Sub(rad As RadioButton, name As String, genOpt As GenOpt)
                    rad.Content = name
                    rad.IsChecked = opt = genOpt
                    AddHandler rad.Checked, Sub()
                                                opt = genOpt
                                                resetGeometry()
                                            End Sub
                    stackPanel.Children.Add(rad)
                End Sub

            addOpt(optMesh1, "All in one mesh", GenOpt.Opt1)
            addOpt(optMesh2, "Many meshes", GenOpt.Opt2)
            addOpt(optMesh3, "One mesh per triangle", GenOpt.Opt3)

            Tools.Children.Add(stackPanel)
        End If

        resetGeometry()

    End Sub

    Private ReadOnly c15024 As New CeeInfo() With {
        .Web = 152,
        .Flange = 64,
        .Lip = 18.5,
        .Thickness = 2.4
    }

    Private ReadOnly rnd As New Random()
    Private ReadOnly sw As New Stopwatch()

    Class CeeInfo
        Public Web As Double
        Public Flange As Double
        Public Lip As Double
        Public Thickness As Double
    End Class

    Private Sub AddCee(posLists As List(Of Point3DCollection), opt As GenOpt, cee As CeeInfo, startPos As Point3D, endPos As Point3D, xdir As Vector3D)
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

        AddCee(posLists, opt, mat, len, cee)
    End Sub

    Private Sub AddCee(posLists As List(Of Point3DCollection), opt As GenOpt, mat As Matrix3D, len As Double, cee As CeeInfo)
        Select Case opt
            Case GenOpt.Opt1
                ' Just 1 list for everything
                If posLists.Count = 0 Then
                    posLists.Add(New Point3DCollection)
                End If

            Case GenOpt.Opt2
                ' Always add a new mesh here
                posLists.Add(New Point3DCollection)

            Case GenOpt.Opt3
                ' Ignore and we'll add a new mesh for each triangle

            Case Else
                Throw New InvalidOperationException($"Unknown opt value: {opt}")
        End Select

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

        For i = 0 To positions.Length - 1
            positions(i) = mat.Transform(positions(i))
        Next

        Dim tri =
            Sub(a As Integer, b As Integer, c As Integer)
                Dim posList As Point3DCollection

                Select Case opt
                    Case GenOpt.Opt1
                        posList = posLists.Last()

                    Case GenOpt.Opt2
                        posList = posLists.Last()

                    Case GenOpt.Opt3
                        ' A new mesh for each triangle
                        posList = New Point3DCollection
                        posLists.Add(posList)

                    Case Else
                        Throw New InvalidOperationException($"Unknown opt value: {opt}")
                End Select

                posList.Add(positions(a))
                posList.Add(positions(b))
                posList.Add(positions(c))
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

    Enum GenOpt
        Opt1
        Opt2
        Opt3
    End Enum

End Class
