implementation module Data.Error

import Text.Show

/// # Instances

instance Show Error where
    show Impossible = "An impossible error occured..."

