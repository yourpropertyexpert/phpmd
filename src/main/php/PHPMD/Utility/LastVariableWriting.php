<?php
/**
 * This file is part of PHP Mess Detector.
 *
 * Copyright (c) Manuel Pichler <mapi@phpmd.org>.
 * All rights reserved.
 *
 * Licensed under BSD License
 * For full copyright and license information, please see the LICENSE file.
 * Redistributions of files must retain the above copyright notice.
 *
 * @author Manuel Pichler <mapi@phpmd.org>
 * @copyright Manuel Pichler. All rights reserved.
 * @license https://opensource.org/licenses/bsd-license.php BSD License
 * @link http://phpmd.org/
 */

namespace PHPMD\Utility;

use InvalidArgumentException;
use OutOfBoundsException;
use PDepend\Source\AST\ASTFormalParameter;
use PDepend\Source\AST\ASTFormalParameters;
use PDepend\Source\AST\ASTType;
use PHPMD\Node\ASTNode;

/**
 * Utility class to provide string checks and manipulations
 */
final class LastVariableWriting
{
    private $variable;

    public function __construct(ASTNode $variable)
    {
        $this->variable = $variable;
    }

    /** @return ASTNode|null */
    public function findInScope(ASTNode $scope)
    {
        $lastWriting = null;
        $name = $this->variable->getImage();

        foreach ($scope->findChildrenOfTypeVariable() as $occurrence) {
            // Only care about occurrences of the same variable
            if ($occurrence->getImage() !== $name) {
                continue;
            }

            // Only check occurrences before, stop when found current node
            if ($occurrence === $this->variable) {
                break;
            }

            $parent = $occurrence->getParent();

            if ($parent->isInstanceOf('AssignmentExpression')) {
                $assigned = $this->getChildIfExist($parent, 0);

                if ($assigned && $assigned->getImage() === $name) {
                    $lastWriting = $this->getChildIfExist($parent, 1);
                }
            }
        }

        return $lastWriting;
    }

    /** @return ASTType */
    public function findInParameters(ASTFormalParameters $parameters)
    {
        $name = $this->variable->getImage();

        /** @var ASTFormalParameter $parameter */
        foreach ($parameters->getChildren() as $parameter) {
            if ($parameter->hasType() && $parameter->getChild(1)->getImage() === $name) {
                return $parameter->getType();
            }
        }

        return null;
    }

    /** @return ASTNode|null */
    private function getChildIfExist($parent, $index)
    {
        try {
            if ($parent instanceof ASTNode) {
                return $parent->getChild($index);
            }
        } catch (OutOfBoundsException $e) {
            // fallback to null
        }

        return null;
    }
}
