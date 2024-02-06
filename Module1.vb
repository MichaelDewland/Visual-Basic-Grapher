Imports System.Data.SqlTypes
Imports System.Net.Http.Headers
Imports System.Runtime.CompilerServices

Module Module1

    ' graph dimensions
    Dim WIDTH As Integer = 30
    Dim HEIGHT As Integer = 30

    ' origin offset
    Dim OFFSET_X As Decimal = 2.5
    Dim OFFSET_Y As Decimal = 2.5

    ' max signed value of each axis
    Dim SCALE As Decimal = 5

    ' calculate offsets for colouring functions
    Dim CALCULATION_OFFSET_X As Decimal = 0.5
    Dim CALCULATION_OFFSET_Y As Decimal = 0.5

    Dim functionDictionary As New Dictionary(Of String, Func(Of Double, Double))

    Sub Main()

        ' Define any useful functions that can be used in an equation
        functionDictionary("sin") = Function(a) Math.Sin(a)
        functionDictionary("cos") = Function(a) Math.Cos(a)
        functionDictionary("tan") = Function(a) Math.Tan(a)

        Dim equations As New List(Of Equation)
        equations.Add(New Axis())
        equations.Add(New Equation("(cos x) / (tan (x)) - y", ConsoleColor.Blue))
        equations.Add(New Equation("y^2 - x^3 + x", ConsoleColor.Green))
        equations.Add(New Equation("(x^2 + 0.5*y^2 - 1)^3 - (x^2 * 0.5*y^3)", ConsoleColor.Magenta))

        For Each equation In equations
            plotEquation(equation)
        Next

        Console.ReadKey()
    End Sub

    Class Tokeniser

        Shared Function tokenize(ByVal equation As String) As List(Of String())

            Dim tokens As New List(Of String())
            Dim shunted As List(Of String())
            Dim splitAtWhitespace As String()

            equation = removeExcessWhitespace(equation)
            splitAtWhitespace = equation.Split(" ")
            tokens = getTokensFromStrings(splitAtWhitespace)
            shunted = shunt(tokens) ' turn them into evaluatable RPN
            Return shunted
        End Function

        Private Shared Function getTokensFromStrings(ByVal strings As String()) As List(Of String())

            Dim tokens As New List(Of String())
            Dim substringTokens As List(Of String())

            For i = 0 To strings.Length - 1

                substringTokens = getTokensInString(strings(i))

                For j = 0 To substringTokens.Count() - 1
                    tokens.Add(substringTokens.Item(j))
                Next
            Next

            Return tokens
        End Function

        Private Shared Function shunt(ByVal tokens As List(Of String())) As List(Of String())
            ' Dijkstra's Shunting Yard algorithm
            ' https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwj0zqSympeEAxXyXUEAHSLFAt4QFnoECCMQAw&url=https%3A%2F%2Fmath.oxford.emory.edu%2Fsite%2Fcs171%2FshuntingYardAlgorithm%2F%23%3A~%3Atext%3DEdsger%2520Dijkstra%2520developed%2520his%2520%2522Shunting%2Cthe%2520operators%2520in%2520the%2520expression.&usg=AOvVaw1lIv7iXYQU3b4p_xFkURLI&opi=89978449
            
            Dim valueQueue As New List(Of String())
            Dim operatorStack As New Stack(Of String())

            Dim precedences As New Dictionary(Of String, Integer)
            precedences.Add("+", 0)
            precedences.Add("-", 0)
            precedences.Add("*", 1)
            precedences.Add("/", 2)
            precedences.Add("Word", 3)
            precedences.Add("^", 4)

            For Each token In tokens

                Dim type As String = token(0)
                Dim value As String = token(1)

                Dim precedence As Integer = getPrecedence(token, precedences)
            
                ' 1. If the incoming symbols is an operand, add to queue
                If type = "Num" OrElse type = "Var" Then
                    valueQueue.Add(token)
                End If
            
                ' 2. If the incoming symbol is a left parenthesis, push it on the stack.
                If type = "LParen" Then
                    operatorStack.Push(token)
                End If

                ' 3. If the incoming symbol is a right parenthesis: discard the right parenthesis, 
                ' pop and print the stack symbols until you see a left parenthesis. Pop the left parenthesis and discard it.
                If type = "RParen" Then
                    While operatorStack.Count > 0 AndAlso operatorStack.Peek()(0) <> "LParen"
                        valueQueue.Add(operatorStack.Pop())
                    End While

                    If operatorStack.Count > 0 AndAlso operatorStack.Peek()(0) = "LParen" Then
                        operatorStack.Pop()
                    End If
                End If

                ' 4. If the incoming symbol is an operator and the stack is empty or contains a left parenthesis on top, push the incoming operator onto the stack.
                If type = "Op" OrElse type = "Word" Then

                    Dim topPrecedence As Integer

                    If operatorStack.Count = 0 Then
                        topPrecedence = 100000
                    Else
                        topPrecedence = getPrecedence(operatorStack.Peek(), precedences)
                    End If

                    ' 5. If the incoming symbol is an operator and has either higher precedence than the operator on the top of the stack, 
                    ' or has the same precedence as the operator on the top of the stack and is right associative, or if the stack is empty, or if the top of the stack is "(" (a floor) -- push it on the stack.
                    If operatorStack.Count = 0 OrElse operatorStack.Peek()(0) = "LParen" Then
                        operatorStack.Push(token)
                    ElseIf precedence > topPrecedence OrElse (precedence = topPrecedence And type = "Word") Then
                        operatorStack.Push(token)

                    ' 6. If the incoming symbol is an operator and has either lower precedence than the operator on the top of the stack,
                    ' or has the same precedence as the operator on the top of the stack and is left associative -- continue to pop the stack until this is not true. Then, push the incoming operator.
                    ElseIf precedence < topPrecedence OrElse (precedence = topPrecedence And type = "Op") Then
                        Do While operatorStack.Count > 0 AndAlso (precedence < topPrecedence OrElse (precedence = topPrecedence And type = "Op"))
                            valueQueue.Add(operatorStack.Pop())
                            If operatorStack.Count > 0 Then
                                topPrecedence = getPrecedence(operatorStack.Peek(), precedences)
                            End If
                        Loop

                        operatorStack.Push(token)
                    End If
                End If
            Next

            ' 7. At the end of the expression, pop and enqueue all operators on the stack.
            While operatorStack.Count > 0
                valueQueue.Add(operatorStack.Pop())
            End While

            Return valueQueue
        End Function

        Private Shared Function getPrecedence(ByVal token As String(), ByVal precedences As Dictionary(Of String, Integer)) As Integer
            If token(0) = "Word" Then
                Return precedences.Item(token(0))
            End If

            If token(0) = "Op" Then
                Return precedences.Item(token(1))
            End If

            Return 0
        End Function

        Private Shared Function getTokensInString(ByVal word As String) As List(Of String())

            Dim tokens As New List(Of String())

            Dim mergedToken As String()
            Dim currentTokenWord As New List(Of String())

            Dim currentType As String = "None"

            For Each c In word

                Dim token As String() = getCharType(c)
                Dim type As String = token(0)
                Dim value As String = token(1)

                If currentType <> "None" Then
                    ' merge any currently considdered tokens
                    If type <> currentType Or type = "LParen" Or type = "RParen" Or type = "Op" Then
                        mergedToken = mergeTokens(currentTokenWord, currentType)
                        tokens.Add(mergedToken)

                        currentTokenWord.Clear()
                    End If
                End If

                currentTokenWord.Add(token)
                currentType = type
            Next
    
            ' merge last token
            If currentTokenWord.Count > 0 Then
                mergedToken = mergeTokens(currentTokenWord, currentType)
                tokens.Add(mergedToken)
            End If

            Return tokens
        End Function

        Private Shared Function getCharType(ByVal c As Char) As String()
            Select Case c
                Case "x", "y"
                    Return {"Var", c}
                Case "0" To "9", "."
                    Return {"Num", c}
                Case "("
                    Return {"LParen", c}
                Case ")"
                    Return {"RParen", c}
                Case "^", "/", "*", "+", "-"
                    Return {"Op", c}
                Case "a" To "z"
                    Return {"Word", c}
                Case Else
                    Return {"Error", c}
            End Select
        End Function

        Private Shared Function mergeTokens(ByVal tokens As List(Of String()), ByVal type As String) As String()

            Dim token As String = ""

            For Each t In tokens
                token += t(1)
            Next

            Return {type, token}
        End Function

        Private Shared Function removeExcessWhitespace(ByVal equation As String) As String
            equation = LCase(equation)
            equation = equation.Trim(" ")
            equation = equation.Replace(vbTab, " ")
            equation = equation.Replace(vbNewLine, " ")

            Dim prev As String

            Do
                prev = equation
                equation = equation.Replace("  ", " ")
            Loop While equation <> prev

            Return equation
        End Function
    End Class

    Class Evaluator

        Dim operatorDictionary As New Dictionary(Of String, Func(Of Double, Double, Double))

        Private equationString As String

        Private RPN As List(Of String())

        Public Sub New(ByVal equationString As String)
            Me.equationString = equationString
            Me.RPN = Tokeniser.tokenize(equationString)

            ' create basic operators
            operatorDictionary("+") = Function(a, b) a + b
            operatorDictionary("-") = Function(a, b) a - b
            operatorDictionary("*") = Function(a, b) a * b
            operatorDictionary("/") = Function(a, b) If(b <> 0, a / b, Double.NaN)
            operatorDictionary("^") = Function(a, b) Math.Pow(a, b)
        End Sub

        Public Function evaluateSign(ByVal x As Decimal, ByVal y As Decimal) As Integer

            Dim calculationStack As New Stack(Of Decimal)

            ' evaluate RPN and return the sign of the resulting calculation
            For Each token In RPN
                Dim type As String = token(0)
                Dim value As String = token(1)

                If type = "Var" Then
                    If value = "x" Then
                        calculationStack.Push(x)
                    ElseIf value = "y" Then
                        calculationStack.Push(y)
                    End If
                ElseIf type = "Num" Then
                    calculationStack.Push(Decimal.Parse(value))
                ElseIf type = "Word" Then
                    Dim value1 As Decimal
                    value1 = calculationStack.Pop()
                    calculationStack.Push(functionDictionary(value)(value1))
                ElseIf type = "Op" Then
                    Dim value2 As Decimal
                    Dim value1 As Decimal
                    value2 = calculationStack.Pop()
                    value1 = calculationStack.Pop()
                    calculationStack.Push(operatorDictionary(value)(value1, value2))
                End If
            Next

            Return Math.Sign(calculationStack.Pop())
        End Function
    End Class

    Class Equation

        Private equationString As String

        Private evaluator As Evaluator

        Private colour As ConsoleColor

        Public Sub New(ByVal equationString As String, ByVal colour As ConsoleColor)
            Me.equationString = equationString
            Me.colour = colour

            evaluator = New Evaluator(equationString)
        End Sub

        Public Overridable Function evaluateSignAt(ByVal x As Decimal, ByVal y As Decimal) As Integer
            Return evaluator.evaluateSign(x, y)
        End Function

        Public Function getColour() As ConsoleColor
            Return Me.colour
        End Function

        Public Overridable Function getCharacter() As String
            Return "X"
        End Function
    End Class

    Class Axis
        Inherits Equation

        Public Sub New()
            MyBase.New("", ConsoleColor.White)
        End Sub

        Public Overrides Function evaluateSignAt(ByVal x As Decimal, ByVal y As Decimal) As Integer
            Return Math.Sign(x * y)
        End Function

        Public Overrides Function getCharacter() As String
            Return "="
        End Function
    End Class

    Sub plotEquation(ByVal equation As Equation)

        Dim offsetX As Decimal
        Dim offsetY As Decimal

        Dim consoleX As Integer
        Dim consoleY As Integer
        Dim total As Integer

        Dim dx As Decimal
        Dim dy As Decimal

        offsetX = CALCULATION_OFFSET_X / WIDTH * SCALE
        offsetY = CALCULATION_OFFSET_Y / HEIGHT * SCALE

        Console.ForegroundColor = equation.getColour()

        For y = 0 To HEIGHT
            For x = 0 To WIDTH

                total = 0

                dx = x / WIDTH * SCALE - OFFSET_X
                dy = y / HEIGHT * SCALE - OFFSET_Y

                ' evaluate the equation at each corner, a mixture of positive and negative signs will total < 4,
                ' therefore the point on the coordinate grid crosses the equation and must be shaded.
                ' This method for shading can only show fine details if the x and y axes are scaled. 
                total += equation.evaluateSignAt(dx - offsetX, dy + offsetY) ' Top left
                total += equation.evaluateSignAt(dx + offsetX, dy + offsetY) ' Top Right
                total += equation.evaluateSignAt(dx - offsetX, dy - offsetY) ' Bottom left
                total += equation.evaluateSignAt(dx + offsetX, dy - offsetY) ' Bottom right

                If Math.Abs(total) < 4 Then

                    consoleX = x * 2
                    consoleY = Console.BufferHeight - y - 1

                    If consoleY >= 0 Then
                        Console.SetCursorPosition(consoleX, consoleY)
                        Console.Write(equation.getCharacter())
                    End If
                End If
            Next
        Next
    End Sub
End Module
