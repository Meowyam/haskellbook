fu1 = const <$> Just "Hello" <*> Just "World"

fu2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
