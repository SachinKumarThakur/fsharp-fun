namespace LibTree

type TreeNode = 
    | Node of value: int * left: TreeNode * right: TreeNode
    | Empty


